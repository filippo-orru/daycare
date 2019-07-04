from flask import jsonify as j, make_response as mr


def make(msg: str, code: int, hints: list = None):
    if hints:
        return mr(j({"message": msg, "hints": hints}), code)
    else:
        return mr(j({"message": msg}), code)


def NotModified():
    return make("Not modified.", 309)


def BadRequest(hints: list = None):
    return make('Bad Request. Check parameters', 400, hints)


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