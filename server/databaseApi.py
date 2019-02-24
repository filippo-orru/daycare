from bson.json_util import dumps, loads
# import bson.json_util
from tools import databaseConnection, actions, tokenManager
import json

dbe = databaseConnection.executeMDb


def login(u, p):
    dbReturn = dbe('users', 'find', {'username': u, 'password': p})['dbReturn']

    try:
        dbReturn = dbReturn[0]
    except IndexError:
        return False

    if 'token' in dbReturn:
        token = str(dbReturn['token'])
    else:
        token = tokenManager.generateToken(u, p)
        # print(token)
        # print(type(token))
        dbe('users', 'update', [{'username': u}, {'$set': {'token': token}}])

    return token


def logout(token):
    username = auth(token)
    if username:
        token = ""
        dbe('users', 'update', [{
            'username': username
        }, {
            '$set': {
                'token': token
            }
        }])
    else:
        return False
    return True


def auth(token, username=None):
    if username:
        try:
            dbReturn = dbe('users', 'find', {
                'token': token,
                'username': username
            })['dbReturn'][0]
        except IndexError:
            return False
    else:
        try:
            dbReturn = dbe('users', 'find', {'token': token})['dbReturn'][0]

        except IndexError:
            return False

    if len(dbReturn) > 0:
        return dbReturn['username']
    else:
        return False


def get(username, component, key=None):  #, _id=-1):
    '''
    input: str(username), str(component)
    return: python object -> loads(dumps(pymongo.cursor.return))
    '''

    success = True
    if component not in [
            'settings', 'attributes', 'categories', 'activities', 'goals',
            'days', 'user'
    ]:
        raise KeyError('Not a valid servercontent type (component)')

    servercontent = dbe('users', 'find', {'username': username})['dbReturn'][0]

    if component != 'user':
        servercontent = servercontent[component]
        # component defined -> go a level deeper

        if key:  # key defined
            try:
                key = int(key)
            except ValueError:
                pass

            # print('type(key)')
            # print(type(key))
            # print('type(servercontent)')
            # print(type(servercontent))
            if (type(key) == int and type(servercontent) == list) \
            or (type(key) == str and type(servercontent) == dict): # key defined and proper usage
                servercontent = servercontent[key]
            else:  # key defined but improper usage
                success = False

    else:  # component = user -> pop problematic objectID index
        servercontent.pop('_id')

    servercontent = json.loads(
        dumps(servercontent))  # convert content to bson and back to pyobj

    if success:
        return servercontent
    else:
        return False


def edit(username, component, usercontent, overwrite=False):
    '''
    input: str(username), str(component), str(usercontent) -> bson-format
    return: int(errorLevel) -> 0/_
    '''
    success = True
    servercontent = get(username, component)  # bson -> pyobj
    usercontent = loads(usercontent)  # bson -> pyobj
    # both contents are now python objects

    if overwrite:
        servercontent = usercontent
    else:
        for key in usercontent.keys():

            if key not in servercontent.keys():
                servercontent[key] = usercontent[key]
            else:
                servercontent[key].update(usercontent[key])

    dbe('users', 'update', [{
        'username': username
    }, {
        '$set': {
            component: servercontent
        }
    }])

    if success:
        return True
    else:
        return False


def delete(component, identifier):
    pass


create = edit  # Create is alias for edit

if __name__ == "__main__":
    username = "fefe"
    user = dbe('users', 'find', {'username': username})
    print(user)
