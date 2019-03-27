from flask import Flask, jsonify as j, request, json, make_response
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
mr = hRes.make


@app.route(apipath + '/login', methods=['POST'])
def login():
    try:
        key = 'username'
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
    except:
        return hRes.InternalServer()

    return response


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
    _user = user

    return mr(j(user), 201)  #, {'Content-Type': 'application/json'})


# , 'PUT'])
@app.route(apipath + '/users/<uID>', methods=['GET', 'PATCH', 'DELETE'])
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
                user = dba.setUserByKey('_id', uID, user)  # update in db
            except:  # if dbError
                return hRes.InternalServer()

        return mr(j(user), 200)  # respond with (not-) modified user

    elif request.method == 'DELETE':
        try:
            dba.deleteUser(uID)
            return mr('Successfully deleted user.', 200)
        except:
            return hRes.InternalServer()


@app.route(apipath + '/users/<uID>/days', methods=['POST', 'GET'])
def days(uID):
    uID = fh.manageAuth(uID, request)
    if not type(uID) == str:
        return uID

    optional = [('limit', int), ('offset', int)]
    limit, offset = fh.assertRequest(request, optional=optional)
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


@app.route('/<uID>/days/<day>', methods=['GET', 'PATCH', 'DELETE'])
def day(uID, day):
    userOrError = fh.manageAuth(uID, request)
    if type(userOrError) == dict:
        _user = userOrError
        uID = _user['id']
    else:
        return userOrError

    _day = dba.getDay(uID, day)

    if request.method == 'GET':
        return mr(j(_user), 200)

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
            dba.deleteDay(day)
            return mr('Successfully deleted day.', 200)
        except:
            return hRes.InternalServer()
    return ""


if __name__ == "__main__":
    app.run('localhost', '5000', debug=False)
