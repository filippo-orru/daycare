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


def defaultUser():
    return {
        'email': '',
        'username': '',
        'password': '',
        'settings': {
            'fullscreen': True,
            'theme': 'dark'
        },
        'goals': [],
        'activities': [],
        'attributes': [],
        'categories': [],
        'token': ''
    }
