import random, base64, databaseApi
try:
    from tools.actions import stringToInt
except ImportError:
    from actions import stringToInt


def addTokenDb(u, p):
    pass


def addToken(session, request):
    # def addToken(key, session, u, p):
    data = request.get_json()
    if 'username' in data and 'password' in data:
        u = data['username']
        p = data['password']
    else:
        return False

    if session.get('user', False):
        user = session.get('user')
        if 'token' in user.keys():
            token = user['token']
        else:
            token = generateToken(u, p)
            # token = str(generateToken(key, u, p))
    else:
        token = generateToken(u, p)
        # token = str(generateToken(key, u, p))

    if token in [None, False]:
        return False
    else:
        # session.set('user', {'token': token})
        return token


def generateToken(u, p):
    u = stringToInt(u)
    p = stringToInt(p)
    # key = stringToInt(key)
    # b = bytes([int(d) for d in str(u * p * key)])
    r = random.randint(10000, 100000)
    b = bytes([int(d) for d in str(u * p * r)])

    return str(base64.urlsafe_b64encode(b))


# def auth(session, token):


def auth(request, session):
    data = request.get_json()
    if 'token' in data:
        token = data['token']
    else:
        return False

    user = session.get('user', False)
    if user:
        print(str(token))
        print(str(user['token']))
        return str(token) == str(user['token'])
    else:
        return False


if __name__ == "__main__":
    # l =
    # print(l)
    # print(generateToken('fefe', '123456'))
    pass
