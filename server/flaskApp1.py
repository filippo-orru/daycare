from flask import Flask, jsonify as j, request, json, make_response as mr, current_app
from flask_cors import CORS
from datetime import datetime

import pymongo

from tools.actions import cleanList, clear

import databaseApi as dba
import flask_helper as fh
import math


app = Flask(__name__,
            template_folder="frontend/templates",
            static_folder="frontend/static")
CORS(app)

apipath = '/api/v3'
hRes = fh.httpResponse
collMethods = ['GET', 'POST', 'DELETE']
itemMethods = ['GET', 'PATCH', 'DELETE']


@app.route('/')
def index():
    return 'daycare api'


@app.route(apipath + '/login', methods=['POST'])
def login():
    body = request.get_json()

    # print(body)
    if not 'password' in body:
        return hRes.BadRequest(['missing required key password'])

    if 'email' in body:
        key = 'email'
    elif 'username' in body:
        key = 'username'
    else:
        return hRes.BadRequest(['missing authentication key ("email" / "username")'])

    value = body[key]
    password = body['password']

    try:
        response = dba.login(key, value, password)
    except ValueError:
        return hRes.make('Login failed. Invalid credentials', 401)

    return mr(j({'token': response}), 200)


@app.route(apipath + '/logout', methods=['POST'])
def logout():
    userOrError = fh.manageAuth('me', request)
    if type(userOrError) == dict:
        _user = userOrError
        uID = _user['_id']
    else:
        return userOrError
    
    _user['token'] = ''
    dba.setUserByKey(_user,'_id',uID)

    return hRes.make('Successfully logged out', 200, ['token was reset'])

@app.route(apipath + '/users', methods=['GET', 'POST'])
def users():

    if request.method == 'GET':
        userOrError = fh.manageAuth('me', request)
        if type(userOrError) == dict:
            _user = userOrError
            uID = _user['_id']
        else:
            return userOrError

        if 'level' in _user:
            if _user['level'] in ['admin', 'mod']:
                users = dba.listUsers()
                return mr(j(users), 200)

        return hRes.Forbidden()

    # critical = [('email', fh.getSchema.emailRe),
    #             ('password', fh.getSchema.passwordRe)]
    # optional = [('username', fh.getSchema.usernameRe)]

    try:
        validBody = fh.assertRequestStrictC(request, fh.getSchema.userPostCt)
    except (KeyError, TypeError):
        return hRes.BadRequest()

    try:
        if 'email' in validBody and 'password' in validBody:
            if 'username' in validBody:
                user = dba.addUser(validBody['email'], validBody['password'], validBody['username'])
            else:
                user = dba.addUser(validBody['email'], validBody['password'])

        else:
            return hRes.BadRequest()
        
    except (ValueError, pymongo.errors.DuplicateKeyError):
    # except (ValueError, pymongo.errors.DuplicateKeyError):
        return hRes.Conflict()
            
    try:
        user['token'] = dba.login('email', validBody['email'], validBody['password'])
    except ValueError:
        return hRes.InternalServer()
    # except:
    #     return hRes.InternalServer()

    return mr(j(user), 201)


@app.route(apipath + '/users/<uID>/setPassword', methods=['POST'])
def userSetPw(uID):
    userOrError = fh.manageAuth(uID, request)
    if type(userOrError) == dict:
        (_user, userSearching) = (userOrError, None)
        uID = _user['_id']
    elif type(userOrError) == tuple:
        (_user, userSearching) = userOrError
    else:
        return userOrError

    schema = {'password': fh.getSchema.userPostCt['password']}
    [userPw, hints, fatal] = fh.assertRequestStrictCH(request, schema)

    if fatal:
        return hRes.BadRequest(hints)
    
    _user['password'] = dba.argon2.hash(userPw['password'])
    _user['token'] = ''
    _user.pop('_id')
    # try:
    dba.setUserByKey(_user,'_id', uID)
    # except:

    return hRes.make('Successfully updated password', 200, ['you need to log in now'] + hints)


@app.route(apipath + '/users/<uID>', methods=itemMethods)
def user(uID):
    userOrError = fh.manageAuth(uID, request)
    if type(userOrError) == dict:
        (_user, userSearching) = (userOrError, None)
        uID = _user['_id']
    elif type(userOrError) == tuple:
        (_user, userSearching) = userOrError
    else:
        return userOrError

    if request.method == 'GET':
        return mr(j(_user), 200)

    elif request.method == 'PATCH':

        [userPatch, hints, fatal] = fh.assertRequestStrictCH(request, fh.getSchema.userCt)
        if fatal:
            return hRes.BadRequest(hints)

        if 'email' in userPatch:
            try:
                possibleOtherUser = dba.getUserByKey('email', userPatch['email'])
                if possibleOtherUser['token'] != _user['token']:
                    return hRes.Conflict()  # email exists
            except KeyError:
                pass

        if 'username' in userPatch:
            try:
                possibleOtherUser = dba.getUserByKey('username', userPatch['username'])
                if possibleOtherUser['token'] != _user['token']:
                    return hRes.Conflict()  # uname exists
            except KeyError:
                pass

        if 'level' in userPatch and 'level' in _user:
            if not (_user['level'] in ['admin', 'mod'] or userSearching):
                return hRes.Forbidden()

        user = dict(_user)  # set newUser = oldUser
        user.update(userPatch)  # update newUser with new key/value Pairs

        if user != _user:
            # try:
            user = dba.setUserByKey(user, '_id')  # update in db
            # except:  # if dbError
            #     return hRes.InternalServer()

        return hRes.make(user, 200, hints)  # respond with (not-) modified user

    elif request.method == 'DELETE':
        dba.deleteUser(uID)
        return hRes.make('Successfully deleted user.', 200)


@app.route(apipath + '/users/<uID>/days', methods=collMethods)
def days(uID):
    userOrError = fh.manageAuth(uID, request)
    if type(userOrError) == dict:
        _user = userOrError
        uID = _user['_id']
    else:
        return userOrError

    if request.method == 'GET':

        limit, offset = None, None

        try:
            limit = fh.clamp(int(request.headers.get('limit')), 1, 500)
        except:
            pass
        try:
            offset = fh.clamp(int(request.headers.get('offset')), 0, 9500)
        except:
            pass

        try:
            _days = dba.getDays(uID, *fh.cleanList(limit, offset))
            return mr(j(_days), 200)

        except ValueError:
            return hRes.BadRequest()
        except KeyError:
            # print("returning empty days")
            # print(_user)
            return mr(j([]), 200)
        

    elif request.method == 'POST':
        # try:
        [day, hints, fatal] = fh.assertRequestStrictCH(request, fh.getSchema.dayCt)
        
        if fatal:
            return hRes.BadRequest(hints)

        for _day in _days:  # todo: inefficient
            if day['date'] == _day['date']:
                return hRes.Conflict()

        day['owner'] = uID
        # day['_id'] = dba.fromObjectId(day['_id'])

        try:
            _day = dict(day)
            dba.addDay(_day)
        except dba.dbC.pmgerrs.DuplicateKeyError:
            return hRes.Conflict()
        except ValueError:
            return hRes.BadRequest()

        return mr(j(day), 201)


@app.route(apipath + '/users/<uID>/days/<date>', methods=itemMethods)
def day(uID, date):
    userOrError = fh.manageAuth(uID, request)
    if type(userOrError) == dict:
        _user = userOrError
        uID = userOrError['_id']
    else:
        return userOrError

    try:
        _day = dba.getDay(uID, date)
    except IndexError:
        return hRes.NotFound()

    if request.method == 'GET':
        return mr(j(_day), 200)

    elif request.method == 'PATCH':
        
        [dayPatch, hints, fatal] = fh.assertRequestStrictCH(request,
                                            fh.getSchema.dayCt)

        if fatal:
            return hRes.BadRequest(hints)    

        day = dict(_day)
        day.update(dayPatch)

        if day != _day:
            dba.setDay(day)

        return mr(j(day), 200)

    elif request.method == 'DELETE':
        dba.deleteDay(_day)
        return hRes.make('Successfully deleted day.', 200)


@app.route(apipath + '/users/<uID>/activities', methods=collMethods)
def activities(uID):
    # return user()
    return fh.collection('activities', request, fh.getSchema.activityCt, uID)


@app.route(apipath + '/users/<uID>/activities/<name>', methods=itemMethods)
def activity(uID, name):
    return fh.item('activities', request, fh.getSchema.activityCt, uID, 'name',
                   name)


@app.route(apipath + '/users/<uID>/attributes', methods=collMethods)
def attributes(uID):
    return fh.collection('attributes', request, fh.getSchema.attributeCt, uID)


@app.route(apipath + '/users/<uID>/attributes/<name>', methods=itemMethods)
def attribute(uID, name):
    return fh.item('attributes', request, fh.getSchema.attributeCt, uID,
                   'name', name)


@app.route(apipath + '/users/<uID>/goals', methods=collMethods)
def goals(uID):
    return fh.collection('goals', request, fh.getSchema.attributeCt, uID)


@app.route(apipath + '/users/<uID>/goals/<name>', methods=itemMethods)
def goal(uID, name):
    return fh.item('goals', request, fh.getSchema.goalCt, uID, 'name', name)


@app.route(apipath + '/users/<uID>/settings', methods=['GET', 'PATCH'])
def settings(uID):
    userOrError = fh.manageAuth(uID, request)
    if type(userOrError) == dict:
        user = userOrError
    else:
        return userOrError

    _settings = user['settings']

    if request.method == 'GET':
        return mr(j(_settings))

    elif request.method == 'PATCH':
        try:
            validBody = fh.assertRequestStrictC(request, fh.getSchema.settingsCt)
        except KeyError:
            return hRes.BadRequest()

        settings = dict(_settings)
        settings.update(validBody)

        if settings != _settings:
            user['settings'] = settings
            dba.setUserByKey(user, '_id')

        return mr(j(settings), 200)


# @app.errorhandler(500)
# def internalServerError(e):
#     return hRes.InternalServer()


@app.errorhandler(400)
def BadRequestError(e):
    return hRes.BadRequest()


@app.errorhandler(405)
def BadMethodError(e):
    # print(e)
    return hRes.BadMethod()


@app.errorhandler(404)
def NotFoundError(e):
    return hRes.NotFound()


if __name__ == "__main__":
    app.run('0.0.0.0', '5000', True)
