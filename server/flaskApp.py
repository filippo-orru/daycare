from flask import Flask, render_template, jsonify as j, request, json, Response, session
from flask_cors import CORS
from tools.actions import cleanList
# from tools import tokenManager

import databaseApi
from datetime import datetime

app = Flask(
    __name__,
    template_folder="frontend/templates",
    static_folder="frontend/static")

CORS(app, resources={r"/api/*": {"origins": "*"}})

app.secret_key = b'ctigsrehulyauul'
# session(app)


@app.route('/')
def index():  # required data: -/-
    dt = datetime.now().strftime('%d.%m.%Y')
    # date = "{0}.{1}.{2}".format(dt.day, dt.month, dt.year)
    date = dt

    return render_template(
        'index.html', user=databaseApi.get('fefe', 'user'), cdate=date)


# Api
apipath1 = '/api/v1/'
apipath2 = '/api/v2/'

validComponents = [
    'days', 'goals', 'attributes', 'activities', 'settings', 'sid', 'username',
    'token'
]
# newapi


@app.route(apipath2 + 'login', methods=['POST'])
def api_login():  # required data: username, pw
    data = request.get_json()
    # print('login request with data:')
    # print(data)
    if 'username' in data and 'password' in data:
        u = data['username']
        p = data['password']
    else:
        return jFalse()

    token = databaseApi.login(u, p)
    # print('token as response to login request:')
    # print(token)
    if token:
        return jTrue({'token': token})
    else:
        return jFalse()


@app.route(apipath2 + 'logout', methods=['POST'])
def api_logout():  # required data: token
    data = request.get_json()
    if 'username' in data and 'password' in data:
        u = data['username']
        p = data['password']

        success = databaseApi.logout(u, p)

    # elif 'token' in data:
    #     token = data['token']
    #     success = databaseApi.logout(token)

    else:
        return jFalse()

    if success:
        return jTrue()
    else:
        return jFalse()


@app.route(apipath2 + 'loggedin', methods=['POST'])
def loggedin():  # required data: token
    if 'token' in request.get_json():
        token = request.get_json()['token']
    else:
        return jFalse()

    auth = databaseApi.authToken(token)

    return jTrue({"loggedin": auth})


@app.route(apipath2 + 'get', methods=['POST'])
@app.route(apipath2 + 'get/<component>', methods=['POST'])
@app.route(apipath2 + 'get/<component>/<key>', methods=['POST'])
def get(component=None, key=None):  # required data: token
    data = request.get_json()
    print('data')
    # print(request.form)

    if 'token' in data and component in validComponents:
        token = data['token']
    else:
        return jFalse()

    username = databaseApi.authToken(token)
    if not username:
        print('invalid token')
        return jFalse()

    if not component:
        component = 'user'

    dbAResponse = databaseApi.get(*cleanList(username, component, key))

    if not dbAResponse:
        return jFalse()

    return jTrue({"content": dbAResponse})


@app.route(apipath2 + 'edit/<component>', methods=['POST'])
@app.route(apipath2 + 'edit/<component>/<key>', methods=['POST'])
def edit(component, key=None):
    data = request.get_json()
    print('data')
    print(data)
    if 'token' in data and 'content' in data and component in validComponents:
        token = data['token']
        content = data['content']
    else:
        print('missing token or content')
        return jFalse()

    username = databaseApi.authToken(token)
    if not username:
        print('invalid token')
        return jFalse()

    overwrite = None
    if 'overwrite' in data:
        overwrite = data['overwrite']

    args = cleanList(username, component, content, key, overwrite)
    # print(args)
    dbAResponse = databaseApi.edit(*args)

    if dbAResponse:
        return jTrue()
    else:
        return jFalse()


def jFalse():
    return j({'success': False})


def jTrue(udi=False):
    di = {'success': True}

    if type(udi) == dict:
        for key, value in udi.items():
            di[key] = value
    elif not udi:
        pass
    else:
        raise TypeError
    return j(di)


# if __name__ == '__main__':
#     app.run(host='127.0.0.1', port=8000, debug=True)