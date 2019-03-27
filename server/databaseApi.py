import bson.errors
from bson.json_util import dumps, loads, ObjectId
from passlib.hash import argon2
# import bson.json_util
from tools import actions, user as userC
from tools import databaseConnection as dbC
from secrets import token_urlsafe
import json

db = dbC.DatabaseConnection()


def login(key, value, p):
    if not authUser(key, value, p):
        return False

    token = token_urlsafe(32)

    db.update('users', [{key: value}, {'$set': {'token': token}}])
    return token


def logout(key, value, p):
    if not authUser(key, value, p):
        return False

    try:
        db.update('users', [{key: value}, {'$set': {'token': ''}}])
    except IndexError:
        return False

    return True


def authUser(key, value, p):
    try:
        result = getUserByKey(key, value, False)
    except IndexError:
        return False

    if argon2.verify(p, result['password']):  # pw correct
        return result['username']
    else:
        return False


def authToken(token, username=None):
    ''' Takes token and optional username and
    outputs username if token and/or username match
    '''

    try:
        result = getUserByKey('token', token)
    except IndexError:
        return False

    if len(result) == 0:
        return False

    serverUsername = result['username']
    if username:
        if serverUsername == username:
            return serverUsername
        else:
            return False
    else:
        return serverUsername


def addUser(email, pwd, uname=''):
    if list(db.find('users', {'email': email})):
        raise ValueError('Email already exists')

    if list(db.find('users', {'username': uname})):
        raise ValueError('Username already exists')

    pwd = argon2.hash(pwd)
    _user = userC.defaultUser()
    _user['email'] = email
    _user['username'] = uname
    _user['password'] = pwd

    uID = db.insert_one('users', _user)

    user = getUserByKey('_id', uID)

    return user


def getUsers(limit=50):
    # try:
    users = db.find('users', {}, limit)

    users = list(map(fromObjectId, users))

    return json.loads(dumps(users))
    # except


def getUserByKey(key, value, popPw=True):
    if key == '_id' and type(value) == str:
        try:
            value = ObjectId(value)
        except bson.errors.InvalidId:
            return None
    user = db.find('users', {key: value})[0]

    user['_id'] = fromObjectId(user['_id'])

    if popPw: user.pop('password')

    return json.loads(dumps(user))


def setUserByKey(key, value, _user):
    db.update('users', [{key: value}, {'$set': _user}])
    newValue = _user[key]
    return getUserByKey(key, newValue)


def deleteUser(uID: str):
    uID = ObjectId(uID)
    db.delete('users', {'_id': uID})


def addDay(_day):
    _day['owner'] = toObjectId(_day['owner'])
    db.insert_one('days', _day)


def getDays(uID, limit=50, offset=0):
    uID = toObjectId(uID)

    days = list(db.find('days', {'owner': uID}, limit, offset, ('date', -1)))

    if len(days) < 1:
        raise KeyError('User not found or has no days')

    _days = []
    for day in days:
        for key, value in day.items():
            day[key] = fromObjectId(value)

    try:
        return days
    except:
        return json.loads(dumps(days))


def getDay(uID, date):
    uID = toObjectId(uID)
    day = db.find('days', {'owner': uID, 'date': date})[0]
    day['_id'] = fromObjectId(day['_id'])
    day['owner'] = fromObjectId(day['owner'])
    return day


def setDay(_day):
    day = dict(_day)
    db.update('days', [{'_id': toObjectId(day['_id'])}, {'$set': day}])


def deleteDay(_day):
    day = dict(_day)
    db.delete('days', {'id': toObjectId(day['_id'])})


def toObjectId(uID):
    try:
        return ObjectId(uID)
    except bson.errors.InvalidId:
        raise ValueError('Invalid ID')


def fromObjectId(field):
    if type(field) == ObjectId:
        return str(field)
    else:
        return field


if __name__ == "__main__":
    # print(getUserByKey('_id', '5c939867c84e20606c713934'))
    # print(getDays('5c632a5b3ee0872e8571d9d4'))
    # de = db.find('users', {'_id': {'$oid': '5c939867c84e20606c713934'}})
    # print(de)
    # print(de[0])
    # print(db.delete_many('users', {'email': {'$exists': False}}))
    # print(
    #     checkAuthentication(
    #         'me', {'token': 'BHlrfRXGbri9Cj7v1qbFyHMaqKxlMgtajLGPi-J4Kvg'}))
    day = {
        "owner": "5c632a5b3ee0872e8571d9d4",
        "date": "2019.03.25",
        "description": "finally decent api",
        "tasks": [{
            "name": "prod",
            "state": "completed"
        }]
    }
    addDay(day)
    pass
