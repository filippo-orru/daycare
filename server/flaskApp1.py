from flask import Flask, jsonify as j, request, json, make_response as mr
from flask_cors import CORS
from datetime import datetime

from tools.actions import cleanList

import databaseApi as dba
import flask_helper as fh

app = Flask(
    __name__,
    template_folder="frontend/templates",
    static_folder="frontend/static")
CORS(app)

apipath = '/api/v3'


@app.route(apipath + '/users', methods=['POST'])
def users():
    critical = [('email', str), ('password', str)]
    optional = [('username', str, True)]
    validBody = fh.assertRequest(request, critical, optional)

    if not validBody:
        fh.err.BadRequest()

    email, password, username = validBody

    try:
        user = dba.addUser(*fh.cleanList(email, password, username))
    except ValueError:
        return fh.err.Conflict()
    except:
        return fh.err.InternalServer()
    _user = user
    user.pop('password')
    # __user = **user
    return mr(j(user), 201)  #, {'Content-Type': 'application/json'})


# , 'PUT'])
@app.route(apipath + '/users/<username>', methods=['GET', 'PATCH', 'DELETE'])
def user(username: str):
    _user = dba.getUserByKey('username', username)
    if not _user:
        return fh.err.NotFound()

    if request.method == 'GET':
        return mr(j(_user), 200)

    elif request.method == 'PATCH':
        # todo: implement 309 not modified:
        # fh.checkBodyMod(user, body) -> mod:True notmod: False
        validBody = fh.assertRequest(request, schema=fh.getSchema.user())
        if not validBody:
            return fh.err.BadRequest()

        user = _user  # set newUser = oldUser
        user.update(validBody)  # update newUser with new key/value Pairs

        if user != _user:  # if newUser notequal old user
            user = dba.setUserByKey('username', username, user)  # update in db
            if not user:  # if dbError
                return fh.err.InternalServer()

        return mr(j(user), 200)  # respond with (not-) modified user


if __name__ == "__main__":
    app.run(host='127.0.0.1', port=5001, debug=True)