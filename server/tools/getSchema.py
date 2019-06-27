emailRe = r"^(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|\"(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])*\")@(?:(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\[(?:(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9]))\.){3}(?:(2(5[0-5]|[0-4][0-9])|1[0-9][0-9]|[1-9]?[0-9])|[a-z0-9-]*[a-z0-9]:(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\])$"
nameRe = r"^[\w -]{3,30}$"
usernameRe = r"^[a-zA-Z0-9][\w-]{1,18}[a-zA-Z0-9]$"
passwordRe = r"^[\w!^$*@!#&%]{8,100}$"
descriptionRe = r"^(?:.|\n){0,500}$"
timezoneRe = r"^[+-](?:0[0-9]|[1][0-4]):(?:00|30|45)$"
dateRe = r"^(20\d{2})(1[012]|0[1-9])(3[01]|[12][0-9]|0[1-9])$"
datetimeRe = r"^(20\d{2})(1[012]|0[1-9])(3[01]|[12][0-9]|0[1-9])(?::(0\d|[01]\d|2[0-3])[0-5]\d)?$"


settingsCt = {
    'fullscreen': (r"^true|false$", 0),
    'theme': (r"^dark|light$", 0),
    'timezone': (timezoneRe, 0)
}

goalCt = {
    'name': (nameRe, 1),
    'description': (descriptionRe, 0),
    'deadline': (datetimeRe, 0)
}

activityCt = {
    'name': (nameRe, 1),
    'description': (descriptionRe, 0),
    'category': (nameRe, 0),
    'related_goals': ([nameRe], 0),
    'productivity_level': (r"^[0-9]|10$", 0)
}

attributeCt = {
    'short': (r"^[A-Z][a-z]?$", 0),
    'name': (nameRe, 1),
    'description': (descriptionRe, 0)
}

categoryCt = {
    'name': (nameRe, 1),
    'description': (descriptionRe,0),
    'productivity_level': (r"^[0-9]|10$",0),
}

timeCt = {
    'pre': (r"^30|([0-2][0-9])|[0-9]$", 0),
    'start': (r"^(((0|1)[0-9])|(?:2[0-3]))[0-5][05]$", 1),
    'end': (r"^(((0|1)[0-9])|(?:2[0-3]))[0-5][05]$", 1),
    'post': (r"^30|([0-2][0-9])|[0-9]$",0)
}

taskCt = {
    'name': (nameRe, 1),
    'state': (r"completed|todo", 1),
    'important': (r"true|false", 0),
    'time': (timeCt, 0)
}
    
dayCt = {
    'date': (dateRe, 1),
    'description': (descriptionRe, 0),
    'attributes': ([nameRe], 0),
    'tasks': ([taskCt], 0)
}

userCt = {
    'email': (emailRe, 0),
    'username': (usernameRe, 0),
    'settings': (settingsCt, 0),
    'goals': ([goalCt], 0),
    'activities': ([activityCt], 0),
    'attributes': ([attributeCt], 0),
    'categories': ([categoryCt], 0),
    'token': (r"^.{32}$", 0)
}

userPostCt = {
    'email': (emailRe, 1),
    'username': (usernameRe, 0),
    'password': (passwordRe, 1)
}
