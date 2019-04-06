from pymongo import MongoClient, ASCENDING, DESCENDING, errors as pmgerrs
from bson import ObjectId
try:
    from tools import parseConfig, actions
except ModuleNotFoundError:
    import parseConfig, actions


class DatabaseConnection():
    _client = None
    _db = None

    def __init__(self):
        path = actions.modPath('databaseConfig.ini')

        if not actions.fileExists(path, False):
            raise FileNotFoundError('Could not find databaseConnection.ini')
        config = parseConfig.parse(path)

        if config['general']['uselocal'] == 'True':
            part = 'mongodb'
        else:
            part = 'mlab'
        print('DEBUG: Creating new connection to database')
        self._client = MongoClient(config[part]['clientUrl'])
        self._db = self._client.get_database()

    # def __enter__(self):
    #     return self

    # def __exit__(self, exec_type, exec_value, traceback):
    #     # MongoClient().get_database()[''].find()
    #     print('DEBUG: Closing connection to database')
    #     self._client.close()

    def __del__(self):
        print('DEBUG: Trying to close connection to db')
        try:
            if self._client:
                self._client.close()
            if self._db:
                self._db = None
            print('SUCCESS: Closed connection to db')
        except TypeError:
            print('ERROR: Failed to close connection to db')
            pass

    def insert_one(self, database, mdbInput):
        return self._db[database].insert_one(mdbInput)

    def insert_many(self, database, mdbInput):
        return self._db[database].insert_many(mdbInput)

    def find(self, database, query, limit=1, offset=0, sort=None):
        '''
        Example: find('users', {"age": {"$gt": 5}}, 5, 1)
        Will find 5 users with age > 5, skipping the first one
        '''
        if sort:
            sortField = sort[0]
            sortDir = ASCENDING if sort[1] == 1 else DESCENDING
            return self._db[database].find(query).skip(offset).limit(
                limit).sort(sortField, sortDir)
        else:
            return self._db[database].find(query).skip(offset).limit(limit)

    def update(self, database, query):
        '''
        Example: update('users', [
            {"age": 18},
            {"$set": {"candrink": "true"}},
            upsert=True])
        '''
        return self._db[database].update(*query)

    def update_many(self, database, query):
        return self._db[database].update_many(*query)

    def delete(self, database, query):
        '''
        Example: delete('users', {"age": {"$lt": 18}})
        '''
        return self._db[database].delete_one(query)

    def delete_many(self, database, query):
        return self._db[database].delete_many(query)

    def drop(self, database):
        return self._db[database].drop()


if __name__ == "__main__":
    # dbc = DatabaseConnection()
    # it3 = list(dbc.find('users', {}))
    # it2 = list(dbc.find('days', {}))
    # it1 = dbc.find('days', {'owner': ObjectId('5c632a5b3ee0872e8571d9d4')})[0]
    # it12 = list(
    #     dbc.find('days', {'owner': ObjectId('5c632a5b3ee0872e8571d9d4')}, 50,
    #              0, ('date', 1)))
    # it13 = list(
    #     dbc.find('days', {'owner': ObjectId('5c632a5b3ee0872e8571d9d4')}, 50,
    #              0, ('date', -1)))
    # print()
    pass
    # sort=('date', -1)))[0]
