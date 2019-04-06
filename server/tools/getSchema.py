def day():
    return {
        'owner': str,
        'date': str,
        'description': str,
        'attributes': [str],
        'tasks': [task()],
    }


def dayPatch():
    return {'description': str, 'attributes': [str], 'tasks': [task()]}


def task():
    return {  # yapf: disable
        'name': str,
        'state': str,
        'important': bool,
        'time': time()
    }


def user():
    return {
        'email': str,
        'username': str,
        'settings': settings(),
        'goals': [goal()],
        'activities': [activity()],
        'attributes': [attribute()],
        'categories': [category()],
        'token': str
    }


def settings():
    return {'fullscreen': bool, 'thema': str, 'timezone': str}


def goal():
    return {'name': str, 'description': str, 'deadline': str}


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


def attribute():
    return {'short': str, 'name': str, 'description': str}


def category():
    return {'name': str, 'description': str, 'productivity_level': int}


def time():
    return {'start': str, 'end': str, 'pre': int, 'post': int}
