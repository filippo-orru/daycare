from flask import Flask, render_template, jsonify as j, request, json, Response, session
from flask_cors import CORS
from tools.actions import cleanList
from tools import tokenManager

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
def index():
    dt = datetime.now().strftime('%d.%m.%Y')
    # date = "{0}.{1}.{2}".format(dt.day, dt.month, dt.year)
    date = dt

    return render_template(
        'index.html', user=databaseApi.get('fefe', 'user'), cdate=date)


# Api
apipath1 = '/api/v1/'
apipath2 = '/api/v2/'

# newapi


@app.route('/elmtest', methods=['POST'])
def elmtest():
    return j({"username": request.get_json()['username']})


@app.route(apipath2 + 'login_db', methods=['POST'])
def api_login_db():
    data = request.get_json()
    if 'username' in data and 'password' in data:
        u = data['username']
        p = data['password']
    else:
        return jFalse()

    token = databaseApi.login(u, p)
    # print(token)
    # print(type(token))
    return jTrue({'token': token})


@app.route(apipath2 + 'logout_db', methods=['POST'])
def api_logout_db():
    if 'token' in request.get_json():
        token = request.get_json()['token']
    else:
        return jFalse()

    if databaseApi.logout(token):
        return jTrue()
    else:
        return jFalse()


@app.route(apipath2 + 'loggedin_db', methods=['POST'])
def loggedin_db():
    if 'token' in request.get_json():
        token = request.get_json()['token']
    else:
        return jFalse()

    auth = databaseApi.auth(token)

    return jTrue({"loggedin": auth})


@app.route(apipath2 + 'login_session', methods=['POST'])
def api_login_session():
    token = tokenManager.addToken(session, request)

    if token:
        session['user'] = {'token': token}
        return jTrue({'token': token})
    else:
        return jFalse()


@app.route(apipath2 + 'logout_session', methods=['POST'])
def api_logout_session():

    if tokenManager.auth(request, session):
        session.pop('user')
        return jTrue()
    else:
        return jFalse()


@app.route(apipath2 + 'loggedin_session', methods=['POST'])
def loggedin_session():
    auth = tokenManager.auth(request, session)

    return jTrue({"loggedin": auth})


@app.route(apipath2 + 'get', methods=['POST'])
@app.route(apipath2 + 'get/<component>', methods=['POST'])
@app.route(apipath2 + 'get/<component>/<key>', methods=['POST'])
def get(component=None, key=None):
    success = True
    _request = request.get_json()

    if 'username' in _request:
        username = _request['username']
    else:
        return jFalse()

    if not component:
        component = 'user'

    content = databaseApi.get(*cleanList(username, component, key))
    if not content:
        return jFalse()

    return jTrue({"content": content})


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