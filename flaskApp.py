from flask import Flask, render_template, jsonify, request, json
app = Flask(__name__)


@app.route('/')
def index():
    return render_template('index.html')


# Api
apipath = '/api/v1/'


# settings
@app.route(apipath + 'settings/edit', methods=['POST'])
def settings_edit():
    errorLevel = 0
    response = {}
    try:
        content = json.loads(request.form['content'])
        response['user'] = content['user']
        response['settings'] = content['settings']
        response['errorLevel'] = errorLevel
    except:
        response = {'errorLevel': 1}

    return jsonify(response)


@app.route(apipath + 'settings/reset', methods=['POST'])
def settings_reset():
    errorLevel = 0
    response = {}
    try:
        content = json.loads(request.form['content'])
        response['user'] = content['user']
        response['errorLevel'] = errorLevel
    except:
        response = {'errorLevel': 1}

    return jsonify({'test': 'Success/failure'})


# end settings


# ++ Start activity ++
@app.route(apipath + 'activity/create', methods=['POST'])
def activity_create():
    errorLevel = 0
    response = {}
    try:
        content = json.loads(request.form['content'])
        response['user'] = content['user']
        response['actitivity'] = content['actitivity']
        response['errorLevel'] = errorLevel
    except:
        response = {'errorLevel': 1}

    return jsonify({'test': content})


@app.route(apipath + 'activity/get', methods=['POST'])
def activity_get():
    errorLevel = 0
    response = {}
    try:
        content = json.loads(request.form['content'])
        response['user'] = content['user']
        response['actitivity'] = content['actitivity']
        response['errorLevel'] = errorLevel
    except:
        response = {'errorLevel': 1}

    return jsonify({'test': content})


@app.route(apipath + 'activity/edit', methods=['POST'])
def activity_edit():
    errorLevel = 0
    response = {}
    try:
        content = json.loads(request.form['content'])
        response['user'] = content['user']
        response['actitivity'] = content['actitivity']
        response['errorLevel'] = errorLevel
    except:
        response = {'errorLevel': 1}

    return jsonify({'test': content})


@app.route(apipath + 'activity/delete', methods=['POST'])
def activity_delete():
    errorLevel = 0
    response = {}
    try:
        content = json.loads(request.form['content'])
        response['user'] = content['user']
        response['actitivity'] = content['actitivity']
        response['errorLevel'] = errorLevel
    except:
        response = {'errorLevel': 1}

    return jsonify({'test': content})


# -- End activity --


# ++ Start category ++
@app.route(apipath + 'category/create', methods=['POST'])
def category_create():
    errorLevel = 0
    response = {}
    try:
        content = json.loads(request.form['content'])
        response['user'] = content['user']
        response['category'] = content['category']
        response['errorLevel'] = errorLevel
    except:
        response = {'errorLevel': 1}

    return jsonify({'test': content})


@app.route(apipath + 'category/get', methods=['POST'])
def category_get():
    errorLevel = 0
    response = {}
    try:
        content = json.loads(request.form['content'])
        response['user'] = content['user']
        response['category'] = content['category']
        response['errorLevel'] = errorLevel
    except:
        response = {'errorLevel': 1}

    return jsonify({'test': content})


@app.route(apipath + 'category/edit', methods=['POST'])
def category_edit():
    errorLevel = 0
    response = {}
    try:
        content = json.loads(request.form['content'])
        response['user'] = content['user']
        response['category'] = content['category']
        response['errorLevel'] = errorLevel
    except:
        response = {'errorLevel': 1}

    return jsonify({'test': content})


@app.route(apipath + 'category/delete', methods=['POST'])
def category_delete():
    errorLevel = 0
    response = {}
    try:
        content = json.loads(request.form['content'])
        response['user'] = content['user']
        response['category'] = content['category']
        response['errorLevel'] = errorLevel
    except:
        response = {'errorLevel': 1}

    return jsonify({'test': content})


# -- End category --


# ++ Start lifegoal ++
@app.route(apipath + 'lifegoal/create', methods=['POST'])
def lifegoal_create():
    errorLevel = 0
    response = {}
    try:
        content = json.loads(request.form['content'])
        response['user'] = content['user']
        response['settings'] = content['settings']
        response['errorLevel'] = errorLevel
    except:
        response = {'errorLevel': 1}

    return jsonify({'test': content})


@app.route(apipath + 'lifegoal/edit', methods=['POST'])
def lifegoal_edit():
    errorLevel = 0
    response = {}
    try:
        content = json.loads(request.form['content'])
        response['user'] = content['user']
        response['settings'] = content['settings']
        response['errorLevel'] = errorLevel
    except:
        response = {'errorLevel': 1}

    return jsonify({'test': content})


@app.route(apipath + 'lifegoal/delete', methods=['POST'])
def lifegoal_delete():
    errorLevel = 0
    response = {}
    try:
        content = json.loads(request.form['content'])
        response['user'] = content['user']
        response['settings'] = content['settings']
        response['errorLevel'] = errorLevel
    except:
        response = {'errorLevel': 1}

    return jsonify({'test': content})


# -- End lifegoal --

if __name__ == '__main__':
    app.run(host='127.0.0.1', port=8000, debug=True)