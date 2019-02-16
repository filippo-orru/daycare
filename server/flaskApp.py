from flask import Flask, render_template, jsonify, request, json, Response
from tools import actions
import databaseApi
app = Flask(__name__)


@app.route('/')
def index():
    return render_template('index.html')


# Api
apipath = '/api/v1/'

# newapi


# settings
@app.route(apipath + '<component>/<action>', methods=['POST'])
def apiAction(component, action):
    errorLevel = 0  # 1: Invalid action; ((2: missing ctype or content))
    response = {}

    print()
    print('======== Api called ========')
    print('component: ' + component + ' action: ' + action)

    if action not in [
            'get', 'edit', 'create', 'delete'
    ] or 'username' not in request.form:  # check for valid action
        errorLevel = 1

    username = request.form['username']
    content = None
    overwrite = None

    # ## ## ##  NEXT: WORK ON INVALID CONTENT ERROR HANDLING -> RESPONSE ERRORLEVEL ETC.

    if action in ['edit']:
        if 'overwrite' in request.form:
            overwrite = (request.form['overwrite'].lower() == 'true')

        if 'content' in request.form:  # request has content   # [action is get and ]
            if not overwrite and 'key' in request.form:  # request has object key
                content = '{{"{0}": {1}}}'.format(request.form['key'],
                                                  request.form['content'])
                # create obj and insert content at <key>
            else:
                content = request.form['content']

    databaseApiParams = actions.cleanList(
        [username, component, content, overwrite])

    dbAResponse = getattr(databaseApi, action)(*databaseApiParams)

    if dbAResponse != '0':
        response['content'] = dbAResponse

    if errorLevel != 0:  # clear response if error
        response = {'errorLevel': errorLevel}
    else:
        response['errorLevel'] = errorLevel

    print('======== Api responded ========')
    print()
    # return Response(jsonify(response), mimetype='application/json')
    return jsonify(response)


# -- End lifegoal --

if __name__ == '__main__':
    app.run(host='127.0.0.1', port=8000, debug=True)