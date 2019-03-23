from flask import jsonify as j, make_response as mr


def errResponse(msg: str, code: int):
    return mr(j({"message": msg}), code)


def InternalServer():
    return errResponse("Internal Server Error", 500)


def Conflict():
    return errResponse(
        "Conflict. If POSTing: resource with that id might already exist.",
        409)


def BadRequest():
    return errResponse('Bad Request. Check parameters', 400)


def NotFound():
    return errResponse('Could not find item', 404)


def NotModified():
    return errResponse("", 309)