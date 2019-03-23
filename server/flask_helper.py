from tools import getSchema
from tools import err


def assertBodyValidity(body, *args):
    '''
    Example (returns True):
    assertBodyValidity({
        'username': '',
        'password': '',
        'a': 4
    },
    'username', 'password',
    ('a', int), ('v', list, True)) # True == optional
    '''
    # if len
    # args = list(args)
    for arg in args:
        if len(arg) == 2:
            if arg[0] in body:
                if type(body[arg[0]]) == arg[1]:
                    continue
            return False
        elif len(arg) == 3:
            if arg[0] in body:
                if type(body[arg[0]]) == arg[1]:
                    continue
            if not arg[2]:  # if not optional
                return False
        else:
            if not arg in body:
                return False
    return True


def filterBody(body, *args):
    '''
    Returns body with only the included and of correct type
    Example:
    filterBody({
            'u': '',
            'i': 5,
            'ii': 5,
            'e': 'e'
        }, ('u', str), 'p', 'i', ('ii', str))
    Returns:
    {'u':'', 'p': '', 'i':5}
    '''
    if not type(body) == dict:
        raise TypeError('Body of invalid type (not dict/list)')

    _body = {}
    for arg in args:
        if len(arg) == 2:
            if arg[0] in body:
                if type(body[arg[0]]) == arg[1]:
                    _body[arg[0]] = body[arg[0]]
        else:
            if arg in body:
                _body[arg] = body[arg]
    return _body


def assertRequest(request,
                  critical: list = None,
                  optional: list = None,
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

    if len(body) < 1:
        return False

    if critical:
        validItems = list(map(filterItems, critical))
        if None in validItems:
            return False

        validItems += map(filterItems, optional)

    elif optional:
        validItems = list(map(filterItems, optional))
    elif schema:
        return assertBodyRecursive(body, schema)
    else:
        return False

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


def toType(stype):
    if type(stype) == type:
        return stype

    if stype == 'str':
        return str
    elif stype == 'int':
        return int
    elif stype == 'bool':
        return toBool
    else:
        return None
    # elif stype == 'list'


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


if __name__ == "__main__":
    # assertRequest(
    #     '{"tru":"e"}',
    #     [],
    # )
    body = {
        "username": "ffactory11",
        "email": "ffactory11@outlook.com",
        "password": "hahalol",
        "optional": 2
    }
    critical = [('email', str), ('password', str)]  #, ('uu', int)]
    optional = [('useruname', str), ('optiuonal', int)]
    cleanBody = assertRequest(body, critical, optional)
    if cleanBody:
        e, p, u, o = cleanBody
        print((e, p, u, o))
    else:
        print(False)
