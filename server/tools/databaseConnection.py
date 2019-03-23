from pymongo import MongoClient
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

        if bool(config['general']['uselocal']) == True:
            part = 'mongodb'
        else:
            part = 'mlab'
        print('DEBUG: Creating new connection to database')
        self._client = MongoClient(config[part]['clientUrl'])
        self._db = self._client.get_database()

    def __enter__(self):
        return self

    def __exit__(self, exec_type, exec_value, traceback):
        # MongoClient().get_database()[''].find()
        print('DEBUG: Closing connection to database')
        self._client.close()

    def __del__(self):
        print('DEBUG: Closing connection to database')
        self._client.close()
        self._db = None

    def insert(self, database, mdbInput):
        return self._db[database].insert(mdbInput)

    def insert_many(self, database, mdbInput):
        return self._db[database].insert_many(mdbInput)

    def find(self, database, mdbInput, limit=1, offset=0):
        '''
        Example: find('users', {"age": {"$gt": 5}}, 5, 1)
        Will find 5 users with age > 5, skipping the first one
        '''
        return self._db[database].find(mdbInput).skip(offset).limit(limit)

    def update(self, database, mdbInput):
        '''
        Example: update('users', [
            {"age": 18},
            {"$set": {"candrink": "true"}},
            upsert=True])
        '''
        return self._db[database].update(*mdbInput)

    def update_many(self, database, mdbInput):
        return self._db[database].update_many(*mdbInput)

    def delete(self, database, mdbInput):
        '''
        Example: delete('users', {"age": {"$lt": 18}})
        '''
        return self._db[database].delete(mdbInput)

    def delete_many(self, database, mdbInput):
        return self._db[database].delete_many(mdbInput)

    def drop(self, database):
        return self._db[database].drop()