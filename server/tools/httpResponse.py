from flask import jsonify as j, make_response as mr


def make(msg: str, code: int):
    return mr(j({"message": msg}), code)


def NotModified():
    return make("", 309)


def BadRequest():
    return make('Bad Request. Check parameters', 400)


def Unauthorized():
    return make('Unauthorized. No valid token provided', 401)


def NotFound():
    return make('Could not find item', 404)


def Conflict():
    return make("Conflict. If POSTing: resource with that id might already exist.", 409)  # yapf: disable


def InternalServer():
    return make("Internal Server Error", 500)