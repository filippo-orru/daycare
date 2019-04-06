from tools import getSchema
from tools import getEmpty
from tools import httpResponse
from tools import htmlErrors
import databaseApi as dba


def assertRequest(request,
                  critical: list = [],
                  optional: list = [],
                  schema=None):
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
    filterItems = lambda x: body[x[0]] if x[0] in body and x[1] == type(body[x[0]]) else None
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
        body = assertBodyRecursive(body, schema)
        if not body:
            raise KeyError('Empty body')
        return body
    else:
        raise LookupError('Not enough arguments provided')

    return validItems


def assertBodyRecursive(body, schema):
    _body = {}
    if type(schema) == type:
        if type(body) == schema:
            return body
    elif not (type(schema) == dict and type(body) == dict):
        raise TypeError('Body and/or schema of invalid type')

    for key, value in schema.items():
        if key in body:
            if type(body[key]) == dict:
                _body[key] = assertBodyRecursive(body[key], value)
            elif type(body[key]) == list:
                _body[key] = []
                for item in body[key]:
                    _body[key].append(assertBodyRecursive(item, value[0]))
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
    # make own errors in htmlErrors.py and implement
    try:
        token = request.headers.get('token')

        if uID == 'me':
            key = 'token'
            value = token
            response = httpResponse.Unauthorized()
        else:
            key = '_id'
            value = uID
            response = httpResponse.NotFound()

        try:
            user = dba.getUserByKey(key, value)
            if user['token'] != token:
                return httpResponse.Unauthorized()
        except IndexError:
            return response
        except KeyError:
            return httpResponse.Unauthorized()
        except ValueError:
            return httpResponse.BadRequest()

    except:
        return httpResponse.InternalServer()
    return user


def collection(part, request, critical, schema, mr, j, uID):
    userOrError = manageAuth(uID, request)
    if type(userOrError) == dict:
        _user = userOrError
    else:
        return userOrError

    _collection = _user[part]
    if request.method == 'GET':
        return mr(j(_collection), 200)

    elif request.method == 'POST':
        try:
            assertRequest(request, critical)
        except KeyError:
            return httpResponse.BadRequest()

        item = assertRequest(request, schema=schema)

        for _item in _collection:
            if _item[critical[0][0]].lower() == item[critical[0][0]].lower():
                return httpResponse.Conflict()

        try:

            _user[part].append(item)

            dba.setUserByKey(_user, '_id')
        except ValueError:
            return httpResponse.BadRequest()
        except:
            return httpResponse.InternalServer()

        return mr(j(_user[part][len(_user[part]) - 1]), 201)

    elif request.method == 'DELETE':
        _user[part] = []
        try:
            dba.setUserByKey(_user, '_id')
        except ValueError:
            return httpResponse.BadRequest()
        except:
            return httpResponse.InternalServer()

        return httpResponse.make('Cleared collection successfully', 200)


def item(part, request, critical, schema, mr, j, uID, partId):
    userOrError = manageAuth(uID, request)
    if type(userOrError) == dict:
        user = userOrError
        uID = userOrError['_id']
    else:
        return userOrError

    _item = None
    i = 0
    for item in user[part]:
        if item[critical[0][0]].lower() == partId.lower():
            _item = item
            break
        i += 1

    if not _item:
        return httpResponse.NotFound()

    if request.method == 'GET':
        return mr(j(_item), 200)

    elif request.method == 'PATCH':
        try:
            validBody = assertRequest(request, schema=schema)
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
    pass
