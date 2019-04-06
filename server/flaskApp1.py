from flask import Flask, jsonify as j, request, json, make_response as mr
from flask_cors import CORS
from datetime import datetime

from tools.actions import cleanList, clear

import databaseApi as dba
import flask_helper as fh
import math

app = Flask(
    __name__,
    template_folder="frontend/templates",
    static_folder="frontend/static")
CORS(app)

apipath = '/api/v3'
hRes = fh.httpResponse
collMethods = ['GET', 'POST', 'DELETE']
itemMethods = ['GET', 'PATCH', 'DELETE']


@app.route(apipath + '/login', methods=['POST'])
def login():
    try:
        key = 'username'
        req = request.get_json()
        critical = [(key, str), ('password', str)]
        value, password = fh.assertRequest(request, critical)
    except KeyError:
        try:
            key = 'email'
            critical = [(key, str), ('password', str)]
            value, password = fh.assertRequest(request, critical)
        except KeyError:
            return hRes.BadRequest()

    try:
        response = dba.login(key, value, password)
    except ValueError:
        return hRes.Unauthorized()
    except:
        return hRes.InternalServer()

    return mr(j({'token': response}), 200)


@app.route(apipath + '/users', methods=['POST'])
def users():
    critical = [('email', str), ('password', str)]
    optional = [('username', str, True)]

    try:
        email, password, username = fh.assertRequest(request, critical,
                                                     optional)
    except KeyError:
        return hRes.BadRequest()

    try:
        user = dba.addUser(*fh.cleanList(email, password, username))
    except ValueError:
        return hRes.Conflict()
    except:
        return hRes.InternalServer()

    return mr(j(user), 201)  #, {'Content-Type': 'application/json'})


# , 'PUT'])
@app.route(apipath + '/users/<uID>', methods=itemMethods)
def user(uID):
    userOrError = fh.manageAuth(uID, request)
    if type(userOrError) == dict:
        _user = userOrError
    else:
        return userOrError

    if request.method == 'GET':
        return mr(j(_user), 200)

    elif request.method == 'PATCH':
        try:
            validBody = fh.assertRequest(request, schema=fh.getSchema.user())
        except KeyError:
            return hRes.BadRequest()

        user = dict(_user)  # set newUser = oldUser
        user.update(validBody)  # update newUser with new key/value Pairs

        if user != _user:
            try:
                user = dba.setUserByKey(user, '_id')  # update in db
            except:  # if dbError
                return hRes.InternalServer()

        return mr(j(user), 200)  # respond with (not-) modified user

    elif request.method == 'DELETE':
        try:
            dba.deleteUser(uID)
            return hRes.make('Successfully deleted user.', 200)
        except:
            return hRes.InternalServer()


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
        return hRes.NotFound()
    except:
        return hRes.InternalServer()

    if request.method == 'GET':
        return mr(j(_days), 200)

    elif request.method == 'POST':
        try:
            fh.assertRequest(request, [('date', str)])  # body must have date
        except KeyError:
            return hRes.BadRequest()

        day = fh.assertRequest(
            request, schema=fh.getSchema.day())  # filter out non-valid keys

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
        except:
            return hRes.InternalServer()

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
            validBody = fh.assertRequest(
                request, schema=fh.getSchema.dayPatch())
        except KeyError:
            return hRes.BadRequest()

        day = dict(_day)
        day.update(validBody)

        if day != _day:
            try:
                dba.setDay(day)
            except:
                return hRes.InternalServer()

        return mr(j(day), 200)

    elif request.method == 'DELETE':
        try:
            dba.deleteDay(_day)
            return hRes.make('Successfully deleted day.', 200)
        except:
            return hRes.InternalServer()


@app.route(apipath + '/users/<uID>/activities', methods=collMethods)
def activities(uID):
    return fh.collection('activities', request, [('name', str)],
                         fh.getSchema.activity(), mr, j, uID)


@app.route(apipath + '/users/<uID>/activities/<name>', methods=itemMethods)
def activity(uID, name):
    return fh.item('activities', request, [('name', str)],
                   fh.getSchema.activity(), mr, j, uID, name)


@app.route(apipath + '/users/<uID>/attributes', methods=collMethods)
def attributes(uID):
    return fh.collection('attributes', request, [('short', str)],
                         fh.getSchema.attribute(), mr, j, uID)


@app.route(apipath + '/users/<uID>/attributes/<short>', methods=itemMethods)
def attribute(uID, short):
    return fh.item('attributes', request, [('short', str)],
                   fh.getSchema.attribute(), mr, j, uID, short)


@app.route(apipath + '/users/<uID>/goals', methods=collMethods)
def goals(uID):
    return fh.collection('goals', request, [('name', str)],
                         fh.getSchema.attribute(), mr, j, uID)


@app.route(apipath + '/users/<uID>/goals/<name>', methods=itemMethods)
def goal(uID, name):
    return fh.item('goals', request, [('name', str)], fh.getSchema.goal(), mr,
                   j, uID, name)


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
            validBody = fh.assertRequest(
                request, schema=fh.getSchema.settings())
        except KeyError:
            return hRes.BadRequest()

        settings = dict(_settings)
        settings.update(validBody)

        if settings != _settings:
            user['settings'] = settings
            try:
                dba.setUserByKey(user, '_id')
            except:
                return hRes.InternalServer()

        return mr(j(settings), 200)


if __name__ == "__main__":
    app.run('localhost', '5000', debug=False)
