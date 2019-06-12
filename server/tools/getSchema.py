emailRe = r"^(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*[a-z0-9]:(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\])$"
nameRe = r"^[\w -]{3,30}$"
usernameRe = r"^[a-zA-Z0-9][\w-]{1,18}[a-zA-Z0-9]$"
passwordRe = r"^[\w!^$*@!#&%]{8,100}$"
descriptionRe = r"^(?:.|\n){0,500}$"
timezoneRe = r"^[+-](?:0[0-9]|[1][0-4]):(?:00|30|45)$"
dateRe = r"^(20\d{2})(1[012]|0[1-9])(3[01]|[12][0-9]|0[1-9])$"
datetimeRe = r"^(20\d{2})(1[012]|0[1-9])(3[01]|[12][0-9]|0[1-9])(?::(0\d|[01]\d|2[0-3])[0-5]\d)?$"


def day():
    return {
        'owner': str,
        'date': str,
        'description': str,
        'attributes': [str],
        'tasks': [task()],
    }


def dayR():
    return {
        'owner': emailRe,
        'date': dateRe,
        'description': descriptionRe,
        'attributes': [nameRe],
        'tasks': [taskR()]
    }


def dayPatch():
    return {'description': str, 'attributes': [str], 'tasks': [task()]}


def dayPatchR():
    r = dayR()
    r.pop('owner')
    r.pop('date')
    return r


def task():
    return {  # yapf: disable
        'name': str,
        'state': str,
        'important': bool,
        'time': time()
    }


def taskR():
    return {  # yapf: disable
        'name': nameRe,
        'state': r"completed|todo",
        'important': r"true|false",
        'time': timeR()
    }


def user():
    return {
        'email': str,
        'username': str,
        'password': passwordRe,
        'settings': settings(),
        'goals': [goal()],
        'activities': [activity()],
        'attributes': [attribute()],
        'categories': [category()],
        'token': str
    }


def userR():
    return {
        'email': emailRe,
        'username': usernameRe,
        'settings': settingsR(),
        'goals': [goalR()],
        'activities': [activityR()],
        'attributes': [attributeR()],
        'categories': [categoryR()],
        'token': r"^.{32}$"
    }


def settings():
    return {'fullscreen': bool, 'theme': str, 'timezone': str}


def settingsR():
    return {
        'fullscreen': r"^true|false$",
        'theme': r"^dark|light$",
        'timezone': timezoneRe
    }


def goal():
    return {'name': str, 'description': str, 'deadline': str}


def goalR():
    return {
        'name': nameRe,
        'description': descriptionRe,
        'deadline': datetimeRe
    }


def activity():
    return {
        'name': str,
        'description': str,
        'category': str,
        'related_goals': [str],
        'productivity_level': int,
        'time': time(),
        'autoedit': bool
    }


def activityR():
    return {
        'name': nameRe,
        'description': descriptionRe,
        'category': nameRe,
        'related_goals': [nameRe],
        'productivity_level': r"^[0-9]|10$"
    }


def attribute():
    return {'short': str, 'name': str, 'description': str}


def attributeR():
    return {
        'short': r"^[A-Z][a-z]?$",
        'name': nameRe,
        'description': descriptionRe
    }


def category():
    return {'name': str, 'description': str, 'productivity_level': int}


def categoryR():
    return {
        'name': nameRe,
        'description': descriptionRe,
        'productivity_level': r"^[0-9]|10$",
    }


def time():
    return {'start': str, 'end': str, 'pre': int, 'post': int}


def timeR():
    return {
        'pre': r"^30|([0-2][0-9])|[0-9]$",
        'start': r"^(((0|1)[0-9])|(?:2[0-3]))[0-5][05]$",
        'end': r"^(((0|1)[0-9])|(?:2[0-3]))[0-5][05]$",
        'post': r"^30|([0-2][0-9])|[0-9]$"
    }
