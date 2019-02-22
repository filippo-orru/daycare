from flask import Flask, render_template, jsonify as j, request, json, Response, session
from flask_cors import CORS
from tools.actions import cleanList
import databaseApi
from datetime import datetime

app = Flask(
    __name__,
    template_folder="frontend/templates",
    static_folder="frontend/static")

CORS(app, resources={r"/api/*": {"origins": "*"}})


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
    # print(request.get_json())
    # print(type(request.get_json()))
    # print(type(request.data[0]))
    # print(request.data[2])
    # print(type(request.data[2]))
    # print(request)
    return j({"username": request.get_json()['username']})


@app.route(apipath2 + 'get', methods=['POST'])
@app.route(apipath2 + 'get/<component>', methods=['POST'])
@app.route(apipath2 + 'get/<component>/<key>', methods=['POST'])
def get(component=None, key=None):
    success = True
    _request = request.get_json()

    if 'username' in _request:
        username = _request['username']
    else:
        return j({'success': False})

    if not component:
        component = 'user'

    content = databaseApi.get(*cleanList(username, component, key))
    if not content:
        success = False

    if success:
        return j({'success': success, "content": content})
    else:
        return j({"success": success})


# if __name__ == '__main__':
#     app.run(host='127.0.0.1', port=8000, debug=True)