from bson.json_util import dumps, loads
# import bson.json_util
from tools import databaseConnection, actions, tokenM, passwordM
import json

dbe = databaseConnection.executeMDb


def login(u, p):
    dbReturn = authUser(u, p)

    if not dbReturn:
        return False

    if 'token' in dbReturn:
        token = str(dbReturn['token'])
    else:
        token = tokenM.generate()
        dbe('users', 'update', [{'username': u}, {'$set': {'token': token}}])

    return token


def logout(u, p):
    if not authUser(u, p):
        return False

    try:
        dbe('users', 'update', [{
            'username': username
        }, {
            '$set': {
                'token': ""
            }
        }])
    except IndexError:
        return False

    return True


def register(u, p):
    pass


def authUser(u, p):
    dbReturn = dbe('users', 'find', {'username': u})
    if not dbReturn:
        return False

    try:
        dbReturn = dbReturn['dbReturn'][0]
    except IndexError:
        return False
    except KeyError:
        return False

    if passwordM.verify(p, dbReturn['password']):  # pw correct
        return dbReturn['username']
    else:
        return False


def authToken(token, username=None):
    ''' Takes token and optional username and
    outputs username if token and/or username match
    '''

    try:
        dbReturn = dbe('users', 'find', {'token': token})['dbReturn'][0]
    except IndexError:
        return False
    if not len(dbReturn) > 0:
        return False

    serverUsername = dbReturn['username']
    if username:
        if serverUsername == username:
            return serverUsername
        else:
            return False
    else:
        return serverUsername


def get(username, component, key=None):  #, _id=-1):
    '''
    input: str(username), str(component)
    return: python object -> loads(dumps(pymongo.cursor.return))
    '''

    if component not in [
            'settings', 'attributes', 'categories', 'activities', 'goals',
            'days', 'user'
    ]:
        raise KeyError('Not a valid servercontent type (component)')

    servercontent = dbe('users', 'find', {'username': username})['dbReturn'][0]

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
    print('username')
    print(username)
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
        print('updating ' + component + ' of ' + username)
        print('New Content: ')
        print(servercontent)
        dbe('users', 'update', [{
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
