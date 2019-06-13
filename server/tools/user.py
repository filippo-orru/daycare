import re


class User():
    def __init__(self, _user):
        if not (type(_user['username']) == str  #noqa
                and type(_user['password']) == str  #noqa
                and type(_user['settings']) == dict  #noqa
                and type(_user['goals']) == list  #noqa
                and type(_user['activities']) == list  #noqa
                and type(_user['attributes']) == list  #noqa
                # type(_user['days']) == str and
                and type(_user['token']) == str):  #noqa
            raise TypeError
        if not (True):  # further checks for schema-correctness
            pass

        self.username = _user['username']
        self.password = _user['password']
        self.settings = _user['settings']
        self.goals = _user['goals']
        self.activities = _user['activities']
        self.attributes = _user['attributes']
        self.days = _user['days']
        self.token = _user['token']


defaultUser = {
    'email': '',
    'username': '',
    'password': '',
    'settings': {
        'fullscreen': False,
        'theme': 'light'
    },
    'goals': [],
    'activities': [],
    'attributes': [],
    'categories': [],
    'confirmed': False,
    'level': 'user',
    'token': ''
}


def validEmail(email: str):
    '''
    Check email for validity.
    Returns valid email or None.
    '''
    emailpattern = r"(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*[a-z0-9]:(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\])"
    match = re.search(emailpattern, email)

    if match:
        return match[0]
    else:
        return False


def validUsername(username: str):
    '''
    Check username for validity.
    Returns valid username or False.
    '''
    usernamepattern = r"^[a-zA-Z0-9][\w-]{1,18}[a-zA-Z0-9]$"
    match = re.search(usernamepattern, username)
    if match:
        return match[0]
    else:
        return False


def validPassword(pw: str):
    '''
    Check password for validity and requirements.
    Returns valid password or False.
    '''
    unallowed_chars = r"[^\w!^$*@!#&%]"
    special_chars = r"[!^$*@!#&%]"
    uppercase = r"[A-Z]"
    lowercase = r"[a-z]"

    if re.search(unallowed_chars, pw) or len(pw) < 8:
        return False

    if re.search(special_chars, pw) or \
        len(pw) >= 16 and re.search(uppercase, pw) and re.search(lowercase, pw):
        return pw
    else:
        return False


def validLevel(lvl: str):
    if lvl in ['admin', 'user', 'mod']:
        return lvl
    else:
        return False