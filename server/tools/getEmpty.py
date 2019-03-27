def day():
    return {
        'owner': '',
        'date': '',
        'description': '',
        'tasks': [task()],
    }


def task():
    return {  # noqua
        'name': '',
        'state': '',
        'important': False,
        'time': time()
    }


def user():
    return {
        'email': '',
        'username': '',
        'settings': settings(),
        'goals': [goal()],
        'activities': [activity()],
        'attributes': [attribute()],
        'categories': [category()],
        'token': ''
    }


def settings():
    return {'fullscreen': False, 'thema': '', 'timezone': ''}


def goal():
    return {'name': '', 'description': '', 'deadline': ''}


def activity():
    return {
        'name': '',
        'description': '',
        'category': '',
        'related_goals': [''],
        'productivity_level': None,
        'time': time(),
        'autoedit': True
    }


def attribute():
    return {'short': '', 'name': '', 'description': ''}


def category():
    return {'name': '', 'description': '', 'productivity_level': None}


def time():
    return {'start': '', 'end': '', 'pre': 0, 'post': 0}
