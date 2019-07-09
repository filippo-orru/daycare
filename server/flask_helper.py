from flask import make_response as mr, jsonify as j

from tools import getSchema
from tools import getEmpty
from tools import httpResponse
from tools import htmlErrors
import databaseApi as dba
import re

def assertRequestStrictCH(request, schema: dict):
    '''
    :shrug:
    '''
    _body = request.get_json()

    body, hints, fatal = assertBodyRecursiveStrictCH(_body, schema)

    # difference = []
    for (key) in _body.keys():
        if key not in body:
            # difference.append(key)
            hints.append("'" + key + "' was supplied but ignored")

    # if not body:
    #     raise KeyError('No key in body matched schema.')
    
    return (body, hints, fatal)


def assertBodyRecursiveStrictCH(body: dict, schema: dict):
    '''
    schema = {
        'key1': (pattern:re, required:bool),
        'key2': { 'key3': (pattern, required)}
        'key3': [ (pattern, required) ]
    }
    '''

    newbody = {}

    hints = []
    fatal = 0

    if not (type(schema) == dict and type(body) == dict):
        hints.append('Body must be a dict.')
        fatal += 1

    for schemakey, schemavalue in schema.items():
        
        pattern = schemavalue[0]
        required = schemavalue[1]
        expl = None
        if len(schemavalue) == 3:
            expl = schemavalue[2]


        if schemakey in body:
            bodyvalue = body[schemakey]

            if type(pattern) == dict:
                if not type(bodyvalue) == dict:
                    hints.append(schemakey + ' must be a dict.')
                    fatal += 1
                
                newbody_, hints_, fatal_ = assertBodyRecursiveStrictC(
                    bodyvalue, pattern) # pattern == dict
                
                newbody[schemakey] = newbody_
                hints += hints_
                fatal += fatal_
            
            elif type(pattern) == list:
                if not type(bodyvalue) == list:
                    hints.append(schemakey + ' must be a list.')
                    fatal += 1
                
                newbody[schemakey] = []
                for item in bodyvalue:

                    newbody_, hints_, fatal_ = assertBodyRecursiveStrictC(item, pattern[0])

                    newbody[schemakey].append(newbody_)
                    hints += hints_
                    fatal += fatal_

                newbody[schemakey] = cleanList(newbody[schemakey])

            else:
                if re.search(pattern, bodyvalue):  # pattern == pattern
                    newbody[schemakey] = bodyvalue
                
                else:
                    if expl:
                        hints.append(expl)
                    else:
                        hints.append(schemakey + ' must match regex: ' + pattern + '.')

                    fatal += 1
        
        elif required:
            hints.append(schemakey + ' is required but missing.')
            fatal += 1
        
    return [ newbody, hints, (fatal > 0) ]


def assertRequestStrictC(request, schema: dict):
    '''
    :shrug:
    '''
    body = request.get_json()

    body = assertBodyRecursiveStrictC(body, schema)
    if not body:
        raise KeyError('No key in body matched schema.')
    return body


def assertBodyRecursiveStrictC(body: dict, schema: dict):
    '''
    schema = {
        'key1': (pattern:re, required:bool),
        'key2': { 'key3': (pattern, required)}
        'key3': [ (pattern, required) ]
    }
    '''

    newbody = {}
    if not (type(schema) == dict and type(body) == dict):
        raise TypeError('Body and/or schema of invalid type')

    for schemakey, schemavalue in schema.items():
        
        pattern = schemavalue[0]
        required = schemavalue[1]


        if schemakey in body:
            bodyvalue = body[schemakey]

            if type(pattern) == dict:
                if not type(bodyvalue) == dict:
                    raise TypeError(schemakey)
                
                newbody[schemakey] = assertBodyRecursiveStrictC(
                    bodyvalue, pattern) # pattern == dict
            
            elif type(bodyvalue) == list and type(pattern) == list:
                if not type(bodyvalue) == list:
                    raise TypeError(schemakey)
                
                newbody[schemakey] = []
                for item in bodyvalue:
                    newbody[schemakey].append(assertBodyRecursiveStrictC(
                        item, pattern[0]))  # pattern == list[pattern]
                newbody[schemakey] = cleanList(newbody[schemakey])

            else:
                if re.search(pattern, bodyvalue):  # pattern == pattern
                    newbody[schemakey] = bodyvalue
                else:
                    raise KeyError(schemakey)
        
        elif required:
            raise KeyError()
        
    return newbody

def assertRequestStrict_(request,
                        critical: list = [],
                        optional: list = [],
                        schema: dict = None):
    '''
    Example:
    body = {
        "username": "ffactory11",
        "email": "ffactory11@outlook.com",
        "password": "hahalol"
    }
    critical = [('email', r"[A-Z0-9]), ('password', r"[A-Z0-9])]
    optional = [('username', r"[A-Z0-9]), ('optional', r"[0-9]")]
    cleanBody = assertRequest(body, critical, optional)
    if cleanBody:
        e, p, u, o = cleanBody
    '''
    body = request.get_json()

    filterItems = lambda x: \
        body[x[0]] if x[0] in body and re.search(x[1], body[x[0]]) else None

    # returns item value if key in body and type matches. Else None

    # if len(body) < 1:
    #     raise

    if critical:
        validItems = list(map(filterItems, critical))
        if None in validItems:
            raise KeyError('Not all critical items found')

        validItems += map(filterItems, optional)

    elif optional:
        validItems = list(map(filterItems, optional))
    elif schema:
        body = assertBodyRecursiveStrict_(body, schema)
        if not body:
            raise KeyError('No key in body matched schema.')
        return body
    else:
        raise LookupError('Not enough arguments provided')

    return validItems


def assertBodyRecursiveStrict_(body: dict, schema: dict):
    _body = {}
    print(body)
    print(schema)
    if not (type(schema) == dict and type(body) == dict):
        raise TypeError('Body and/or schema of invalid type')

    for key, value in schema.items():
        if key in body:
            if type(body[key]) == dict:
                _body[key] = assertBodyRecursiveStrict_(body[key],
                                                       value)  # value == dict
            elif type(body[key]) == list:
                _body[key] = []
                for item in body[key]:
                    _body[key].append(assertBodyRecursiveStrict_(
                        item, value[0]))  # value == list[pattern]
                _body[key] = cleanList(_body[key])
            else:
                # item = tryConvert(body[key])
                if re.search(value, body[key]):  # value == pattern
                    _body[key] = body[key]
    return _body


def assertRequest_(request,
                  critical: list = [],
                  optional: list = [],
                  schema: object = None):
    '''
    Example:
    body = {
        "username": "ffactory11",
        "email": "ffactory11@outlook.com",
        "password": "hahalol"
    }
    critical = [('email', str), ('password', str)]
    optional = [('username', str, True), ('optional', int, True)]
    cleanBody = assertRequest(body, critical, optional)
    if cleanBody:
        e, p, u, o = cleanBody
    '''
    body = request.get_json()
    # body = request
    filterItems = lambda x: body[x[0]] if x[0] in body and x[1] == type(body[x[
        0]]) else None
    # returns item value if key in body and type matches. Else None

    # if len(body) < 1:
    #     raise

    if critical:
        validItems = list(map(filterItems, critical))
        if None in validItems:
            raise KeyError('Not all critical items found')

        validItems += map(filterItems, optional)

    elif optional:
        validItems = list(map(filterItems, optional))
    elif schema:
        body = assertBodyRecursive_(body, schema)
        if not body:
            raise KeyError('Empty body')
        return body
    else:
        raise LookupError('Not enough arguments provided')

    return validItems


def assertBodyRecursive_(body, schema):
    _body = {}
    if type(schema) == type:
        if type(body) == schema:
            return body
    elif not (type(schema) == dict and type(body) == dict):
        raise TypeError('Body and/or schema of invalid type')

    for key, value in schema.items():
        if key in body:
            if type(body[key]) == dict:
                _body[key] = assertBodyRecursive_(body[key], value)
            elif type(body[key]) == list:
                _body[key] = []
                for item in body[key]:
                    _body[key].append(assertBodyRecursive_(item, value[0]))
                _body[key] = cleanList(_body[key])
            else:
                item = tryConvert(body[key])
                # if type(body[key]) == value:
                #     _body[key] = body[key]
                if type(item) == value:
                    _body[key] = item
    return _body


def cleanList(*uinput):
    '''
    Removes any empty or None items from the list
    '''
    if len(uinput) == 1:
        uinput = uinput[0]
    return list(filter(None, uinput))


def toBool(string: str):
    if string.lower() == 'true':
        return True
    elif string.lower() == 'false':
        return False
    else:
        raise ValueError('Neither true nor false')


def tryConvert(value):
    if not type(value) == str:
        return value
    try:
        return int(value)
    except ValueError:
        pass
    try:
        return toBool(value)
    except ValueError:
        pass
    return value


def clamp(n, smallest, largest):
    try:
        n = int(n)
    except:
        return None
    if not n:
        return None
    return max(smallest, min(n, largest))


def manageAuth(uID, request):
    '''
    Searches database for user and returns appropriate http errors if not found etc.
    '''

    token = request.headers.get('token')

    brq = httpResponse.BadRequest

    if uID == 'me':  # search by token
        key = 'token'
        value = token
        errorResponse = httpResponse.Unauthorized()

        try:
            if not re.search(getSchema.tokenRe, value): # token < 50 chars
                return brq(['token of invalid format'])
        except:
            return brq(['missing token'])

    else:  # search by id
        key = '_id'
        value = uID
        errorResponse = httpResponse.NotFound()

        if value in ['', None]:
            return brq(['id must not be empty'])

    try:
        user = dba.getUserByKey(key, value)
        if user['token'] != token:  # user searching =/= user searched for
            userSearching = dba.getUserByKey('token', token)

            if not 'level' in userSearching:
                return httpResponse.Forbidden()

            if userSearching['level'] in ['admin', 'mod']:
                return (user, userSearching)  # userSearching is admin or mod
            else:
                return httpResponse.Forbidden()
                # userSearching exists but insufficient privileges

    except IndexError:
        return errorResponse
    except KeyError:
        return httpResponse.Unauthorized()
    except ValueError:
        return httpResponse.BadRequest()

    return user


def collection(part, request, schema, uID):
    userOrError = manageAuth(uID, request)
    if type(userOrError) == dict:
        (_user, userSearching) = (userOrError, None)
        uID = _user['_id']
    elif type(userOrError) == tuple:
        (_user, userSearching) = userOrError
    else:
        return userOrError

    _collection = _user[part]
    if request.method == 'GET':
        return mr(j(_collection), 200)

    elif request.method == 'POST':
        try:
            # assertRequest(request, critical)
            item = assertRequestStrictC(request, schema)
        except KeyError:
            return httpResponse.BadRequest()

        for _item in _collection:
            for schemakey in schema.keys():
                    if not schemakey[1]:
                        continue
                    else:
                        if _item[schemakey[0]].lower() == item[schemakey[0]].lower():
                            return httpResponse.Conflict()

        try:

            _user[part].append(item)

            dba.setUserByKey(_user, '_id')
        except ValueError:
            return httpResponse.BadRequest()

        return mr(j(_user[part][len(_user[part]) - 1]), 201)

    elif request.method == 'DELETE':
        _user[part] = []
        try:
            dba.setUserByKey(_user, '_id')
        except ValueError:
            return httpResponse.BadRequest()

        return httpResponse.make('Cleared collection successfully', 200)


def item(part, request, schema, uID, identifier, partId):
    userOrError = manageAuth(uID, request)
    if type(userOrError) == dict:
        user = userOrError
        uID = userOrError['_id']
    else:
        return userOrError

    _item = None
    i = 0
    for item in user[part]:
        if item[identifier].lower() == partId.lower():
            _item = item
            break
        i += 1

    if not _item:
        return httpResponse.NotFound()

    if request.method == 'GET':
        return mr(j(_item), 200)

    elif request.method == 'PATCH':
        try:
            validBody = assertRequestStrictC(request, schema)
        except (KeyError, TypeError):
            return httpResponse.BadRequest()

        item = dict(_item)
        item.update(validBody)

        # item[critical[0][0]] = item[critical[0][0]]

        user[part][i] = item

        if item != _item:
            try:
                dba.setUserByKey(user, '_id')
            except ValueError:
                return httpResponse.BadRequest()
            except:
                return httpResponse.InternalServer()

        return mr(j(item), 200)

    elif request.method == 'DELETE':
        user[part].pop(0)
        try:
            dba.setUserByKey(user, '_id')
            return httpResponse.make(
                'Successfully deleted item from ' + part, 200) # yapf: disable
        except ValueError:
            return httpResponse.BadRequest()
        except:
            return httpResponse.InternalServer


if __name__ == "__main__":
    body = {
        "attributes": [], 
        "date": "20190611",
        "description": "",
        "tasks": [
        {
            "name": "",
            "state": "todo"
        }
        ]
    }
    print(assertBodyRecursiveStrictC(body, getSchema.dayCt))
