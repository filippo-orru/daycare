from bson.json_util import dumps, loads
# import bson.json_util
from tools import databaseConnection
import json

dbe = databaseConnection.executeMDb


def get(username, component):  #, _id=-1):
    '''
    input: str(username), str(component)
    return: python object -> loads(dumps(pymongo.cursor.return))'''

    if component not in [
            'settings', 'attributes', 'categories', 'activities', 'goals',
            'days'
    ]:
        raise KeyError('Not a valid servercontent type (component)')

    servercontent = dbe('users', 'find',
                        {'username': username})['dbReturn'][0][component]

    servercontent = loads(dumps(servercontent))

    # if _id >= 0:
    #     return servercontent[_id]

    return servercontent


def edit(username, component, usercontent):
    '''
    input: str(username), str(component), str(usercontent) -> bson-format
    return: int(errorLevel) -> 0/_
    '''
    servercontent = get(username, component)  # bson -> pyobj
    usercontent = loads(usercontent)  # bson -> pyobj
    # both contents are now python objects

    print('servercontent')
    print(servercontent)
    print('usercontent')
    print(usercontent)
    if len(usercontent) > len(servercontent):
        servercontent.append(usercontent)
    else:
        i = 0
        while i < len(
                usercontent):  # Merge new into old servercontent (overwrite)
            servercontent[i].update(usercontent[i])
            i += 1

    dbe('users', 'update', [{
        'username': username
    }, {
        '$set': {
            component: servercontent
        }
    }])
    return 0


def delete(component, identifier):
    pass


create = edit  # Create is alias for edit

if __name__ == "__main__":
    print('running from main')
    ucontent = get('fefe', 'goals')
    print(ucontent)
    # ucontent[0]['description'] = 'Ich will meinen Führerschein endlich machen'

    data = '[{"_id": ObjectId("5c632d5d3ee0872e8571d9d5"), "name": "Führerschein", "description": "123 Ich will meinen Führerschein endlich machen", "timelimit": datetime.datetime(2019, 7, 24, 22, 0)}]'
    data = '[{"_id": {"$oid": "5c632d5d3ee0872e8571d9d5"},"name":"F\u00fchrerschein","description":"123 Ich will meinen F\u00fchrerschein endlich machen","timelimit": {"$date": 1564005600000}}]'

    edit('fefe', 'goals', data)

# def create(component, servercontent):
#     dbe('users', 'update', [{
#         'username': servercontent['username']
#     }, {
#         '$set': {
#             'activities': servercontent[activity]
#         }
#     }, True])