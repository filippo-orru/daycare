from bson.json_util import dumps, loads, ObjectId
from passlib.hash import argon2
# import bson.json_util
from tools import actions, tokenM, user as userC
from tools.databaseConnection import DatabaseConnection
import json

dbc = DatabaseConnection()


def login(u, p):
    result = authUser(u, p)

    if not result:
        return False

    token = tokenM.generate()
    dbc.update('users', [{'username': u}, {'$set': {'token': token}}])
    return token


def logout(u, p):
    if not authUser(u, p):
        return False

    try:
        dbc.update('users', [{'username': u}, {'$set': {'token': ""}}])
    except IndexError:
        return False

    return True


def addUser(email, pwd, uname=''):
    # if list(dbc.find('users', {'email': email})):
    #     raise ValueError('Email already exists')

    # if list(dbc.find('users', {'username': uname})):
    #     raise ValueError('Username already exists')

    pwd = argon2.hash(pwd)
    _user = userC.defaultUser()
    _user['email'] = email
    _user['username'] = uname
    _user['password'] = pwd
    uID = dbc.insert('users', _user)
    user = getUserByKey('_id', uID)

    if user:
        return user
    else:
        raise IndexError('Cannot find created user.')


def authUser(u, p):
    result = getUserByKey('username', u)
    if not result:
        return False

    if argon2.verify(p, result['password']):  # pw correct
        return result['username']
    else:
        return False


def authToken(token, username=None):
    ''' Takes token and optional username and
    outputs username if token and/or username match
    '''

    result = getUserByKey('token', token)

    if len(result) == 0:
        return False

    serverUsername = result['username']
    if username:
        if serverUsername == username:
            return serverUsername
        else:
            return False
    else:
        return serverUsername


def getUserByKey(key, value, popPw=True):
    try:
        user = dbc.find('users', {key: value})[0]
        if popPw: user.pop('_id')
    except IndexError:
        return None
    return json.loads(dumps(user))


def setUserByKey(key, value, _user):
    try:
        dbc.update('users', [{key: value}, {'$set': _user}])
        newValue = _user[key]
    except Exception as e:
        print(e)
        return False
    return getUserByKey(key, newValue)


def get2(username, component, key=None):  #, _id=-1):
    '''
    input: str(username), str(component)
    return: python object -> loads(dumps(pymongo.cursor.return))
    '''

    if component not in [
            'settings', 'attributes', 'categories', 'activities', 'goals',
            'days', 'user'
    ]:
        raise KeyError('Not a valid servercontent type (component)')

    servercontent = dbc.find('users', {'username': username})[0]

    if component != 'user':
        servercontent = servercontent[component]
        # print('servercontent')
        # print(servercontent)
        # component defined -> go a level deeper

        if key:  # key defined
            key = actions.keyOkay(key, servercontent)
            if key:  # key proper usage
                try:
                    servercontent = servercontent[key]
                except KeyError:
                    return False
            else:  # key defined but improper usage
                return False

    else:  # component = user -> pop problematic objectID index
        servercontent.pop('_id')

    servercontent = json.loads(
        dumps(servercontent))  # convert content to bson and back to pyobj

    return servercontent


def edit(username, component, usercontent, key=None, overwrite=False):
    '''
    input: str(username), str(component), str(usercontent),
        opt str/int(key) opt overwrite(bool)
    return: success: True/False
    '''
    servercontent = get(username, component)
    if not servercontent:  # get returned error
        return False

    if type(usercontent) == str:  # usercontent is json string
        usercontent = loads(usercontent)  # bson -> dict
    elif type(usercontent) == dict:  # usercontent already dict
        pass
    else:  # usercontent has invalid type
        return False

    # both contents are python objects

    if overwrite:
        servercontent = usercontent

    else:  # upsert in case of non-overwrite
        if key:  # key defined
            print('key')
            print(key)
            print('servercontent')
            print(servercontent)
            if key in servercontent:  # key in servercont
                # contpart = servercontent[key]  # set contpart
                if actions.keyOkay(key,
                                   servercontent):  # key in scont -> update
                    servercontent[key].update(usercontent)

            else:  # key not in servercontent -> create new
                servercontent[key] = usercontent

        else:  # no key defined
            for uKey in usercontent.keys():  # loop through dicts and upsert
                if uKey not in servercontent.keys():
                    # key not in scont -> create
                    servercontent[uKey] = usercontent[uKey]
                else:  # key in scont -> update
                    servercontent[uKey].update(usercontent[key])

    try:
        dbc.update('users', [{
            'username': username
        }, {
            '$set': {
                component: servercontent
            }
        }])
    except:
        return False

    return True


def delete(component, identifier):
    pass


create = edit  # Create is alias for edit

if __name__ == "__main__":
    print(addUser('email', 'hahalol'))
