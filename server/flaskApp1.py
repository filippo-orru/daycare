from flask import Flask, jsonify as j, request, json, make_response as mr, current_app
from flask_cors import CORS
from datetime import datetime

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

    if not 'password' in body:
        return hRes.BadRequest()

    if 'email' in body:
        key = 'email'
    elif 'username' in body:
        key = 'username'
    else:
        return hRes.BadRequest()

    value = body[key]
    password = body['password']

    try:
        response = dba.login(key, value, password)
    except ValueError:
        return hRes.make('Login failed. Invalid credentials', 401)

    return mr(j({'token': response}), 200)


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

    critical = [('email', fh.getSchema.emailRe),
                ('password', fh.getSchema.passwordRe)]
    optional = [('username', fh.getSchema.usernameRe)]

    try:
        email, password, username = fh.assertRequestStrict(
            request, critical, optional)
    except (KeyError, TypeError):
        return hRes.BadRequest()

    try:
        user = dba.addUser(*fh.cleanList(email, password, username))
    except ValueError:
        return hRes.Conflict()
    # except:
    #     return hRes.InternalServer()

    return mr(j(user), 201)


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
        try:
            schema = fh.getSchema.userR()
            schema.pop('token')

            validBody = fh.assertRequestStrict(request, schema=schema)
        except KeyError:
            return hRes.BadRequest(
            )  # request does not match schema. -> Reject

        if 'email' in validBody:
            try:
                if dba.getUserByKey('email', validBody['email']):
                    return hRes.Conflict()  # email exists
            except KeyError:
                pass

        if 'username' in validBody:
            try:
                if dba.getUserByKey('username', validBody['username']):
                    return hRes.Conflict()  # uname exists
            except KeyError:
                pass

        if 'level' in validBody and 'level' in _user:
            if not (_user['level'] in ['admin', 'mod'] or userSearching):
                return hRes.Forbidden()

        user = dict(_user)  # set newUser = oldUser
        user.update(validBody)  # update newUser with new key/value Pairs

        if user != _user:
            # try:
            user = dba.setUserByKey(user, '_id')  # update in db
            # except:  # if dbError
            #     return hRes.InternalServer()

        return mr(j(user), 200)  # respond with (not-) modified user

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

    limit = request.headers.get('limit')
    offset = request.headers.get('offset')
    if limit and offset:
        limit = fh.clamp(limit, 1, 500)
        offset = fh.clamp(offset, 0, 9500)

    try:
        _days = dba.getDays(uID, *fh.cleanList(limit, offset))
    except ValueError:
        return hRes.BadRequest()
    except KeyError:
        print("returning empty days")
        print(_user)
        return mr(j([]), 200)

    if request.method == 'GET':
        return mr(j(_days), 200)

    elif request.method == 'POST':
        try:
            fh.assertRequestStrict(
                request,
                [('date', fh.getSchema.dateRe),
                 ('owner', fh.getSchema.emailRe)])  # body must have date
        except KeyError:
            return hRes.BadRequest()

        day = fh.assertRequestStrict(
            request, schema=fh.getSchema.dayR())  # filter out non-valid keys

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
        try:
            validBody = fh.assertRequestStrict(request,
                                               schema=fh.getSchema.dayPatchR())
        except KeyError:
            return hRes.BadRequest()

        day = dict(_day)
        day.update(validBody)

        if day != _day:
            dba.setDay(day)

        return mr(j(day), 200)

    elif request.method == 'DELETE':
        dba.deleteDay(_day)
        return hRes.make('Successfully deleted day.', 200)


@app.route(apipath + '/users/<uID>/activities', methods=collMethods)
def activities(uID):
    # return user()
    return fh.collection('activities', request,
                         [('name', fh.getSchema.nameRe)],
                         fh.getSchema.activityR(), uID)


@app.route(apipath + '/users/<uID>/activities/<name>', methods=itemMethods)
def activity(uID, name):
    return fh.item('activities', request, fh.getSchema.activityR(), uID,
                   'name', name)


@app.route(apipath + '/users/<uID>/attributes', methods=collMethods)
def attributes(uID):
    return fh.collection('attributes', request,
                         [('name', fh.getSchema.nameRe),
                          ('short', fh.getSchema.attributeR()['short'])],
                         fh.getSchema.attributeR(), uID)


@app.route(apipath + '/users/<uID>/attributes/<name>', methods=itemMethods)
def attribute(uID, name):
    return fh.item('attributes', request, fh.getSchema.attributeR(), uID,
                   'name', name)


@app.route(apipath + '/users/<uID>/goals', methods=collMethods)
def goals(uID):
    return fh.collection('goals', request, [('name', fh.getSchema.nameRe)],
                         fh.getSchema.attributeR(), uID)


@app.route(apipath + '/users/<uID>/goals/<name>', methods=itemMethods)
def goal(uID, name):
    return fh.item('goals', request, fh.getSchema.goalR(), uID, 'name', name)


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
            validBody = fh.assertRequestStrict(request,
                                               schema=fh.getSchema.settingsR())
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


if __name__ == "__main__":
    app.run('0.0.0.0', '5000', True)
