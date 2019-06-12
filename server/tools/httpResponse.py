from flask import jsonify as j, make_response as mr


def make(msg: str, code: int):
    return mr(j({"message": msg}), code)


def NotModified():
    return make("Not modified.", 309)


def BadRequest(invalid_fields=[]):
    text = ""
    if invalid_fields != []:
        text = ". invalid fields: " + str(invalid_fields)

    return make('Bad Request. Check parameters' + text, 400)


def Unauthorized():
    return make('Unauthorized. No valid token provided', 401)


def Forbidden():
    return make('Forbidden. Valid token but insufficient privileges', 403)


def NotFound():
    return make('Could not find item', 404)


def Conflict():
    return make("Conflict. Resource with that id might already exist", 409)  # yapf: disable


def InternalServer():
    return make("Internal Server Error", 500)