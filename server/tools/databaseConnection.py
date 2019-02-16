from pymongo import MongoClient
# from bson import ObjectId
try:
    from tools import parseConfig, actions
except ModuleNotFoundError:
    import parseConfig, actions
#import pymysql.cursors

# sys.path.insert(1, os.path.join(sys.path[0], '..'))

connectedToDb = False
client = None
db = None
useLocalDatabase = True


def ConnectionToDb():
    global connectedToDb

    if connectedToDb == False:
        return connectToDb()
    else:
        return 1


def connectToDb():
    global client, db, connectedToDb, useLocalDatabase
    # print('Connecting to database')

    # username = str(parse()['mlab']['username'])
    # password = urllib.parse.quote_plus(parse()['mlab']['password'])

    if actions.fileExists('databaseConfig.ini'):
        if useLocalDatabase:
            actions.log('Using local database')
            client = MongoClient(parseConfig.parse()['mongodb']['clientUrl'])
        else:
            actions.log('Using remote database')
            client = MongoClient(parseConfig.parse()['mlab']['clientUrl'])
    else:
        actions.log('cant connect to db inner', 'Error')
        return 0
    db = client.get_database()
    connectedToDb = True
    return 1


def executeMDb(database,
               dbAction,
               mongoDbInput,
               rowCount=1,
               sortStr=None,
               ordered=True):
    errorCode = 0  #0: undef | 1: success | 2: no valid action | 3: empty input string
    dbReturn = None
    global db, useLocalDatabase
    errorCode = ConnectionToDb()
    if useLocalDatabase:
        localString = 'local'
    else:
        localString = 'remote'
    actions.log('Executing {0} on {1} db {2}.'.format(dbAction, localString,
                                                      database))
    if errorCode == 0:
        actions.log('Cant connect to db', 'Error')
        return {'errorCode': errorCode}
    #print(dbAction)
    if mongoDbInput == None:  #or _input == ''
        errorCode = 3
    else:
        saneStr = mongoDbInput

        if dbAction == 'insert':
            db[database].insert(saneStr)
        elif dbAction == 'update':
            try:
                db[database].update(saneStr[0], saneStr[1], saneStr[2])
            except (KeyError, IndexError):
                db[database].update(saneStr[0], saneStr[1])
        elif dbAction == 'updatemany':
            try:
                db[database].update_many(saneStr[0], saneStr[1], saneStr[2])
            except (KeyError):
                db[database].update_many(saneStr[0], saneStr[1])
        elif dbAction == 'find':
            if rowCount == 'all':
                if sortStr != None:
                    dbReturn = db[database].find(saneStr).sort(
                        sortStr[0], sortStr[1])
                else:
                    dbReturn = db[database].find(saneStr)
            else:
                dbReturn = db[database].find(saneStr).limit(rowCount)
        # elif dbAction == 'findChildren':
        #     dbReturn = db[database].find(saneStr).children
        elif dbAction == 'delete':
            dbReturn = db[database].delete(saneStr)
        elif dbAction == 'deletemany':
            dbReturn = db[database].delete_many(saneStr)
        elif dbAction == 'drop':
            db[database].drop()
        elif dbAction == 'bulk':
            db[database].bulk_write(saneStr, ordered=ordered)
        else:
            errorCode = 2

        errorCode = 1
    if dbReturn != None:
        return {'errorCode': errorCode, 'dbReturn': dbReturn}
    else:
        return {'errorCode': errorCode}


def executeBulkMDb(database, dbInputList):  #, abAction
    errorCode = 0  #0: undef | 1: success | 2: no valid action | 3: empty input string
    #print(database)
    #print(dbAction)
    #print(str(mongoDbInput))
    if dbInputList == None or len(dbInputList) == 0:  #or _input == ''
        errorCode = 3
    else:
        ConnectionToDb()
        global db
        collection = db[database]
        rawOperations = sanitizeInput(dbInputList)
        dbReturn = None
        operations = []
        # if len(operations) <= 1:
        #     executeMDb()
        # operations = []
        for rawOperation in rawOperations:

            operations.append(rawOperation)

            # Send once every 1000 in batch
            if (len(operations) == 1000):
                actions.log('sending 1000 to db')
                collection.bulk_write(operations, ordered=False)
                operations = []

        if (len(operations) > 0):
            actions.log('sending rest to db')
            collection.bulk_write(operations, ordered=False)

        errorCode = 1
    if dbReturn != None:
        return {'errorCode': errorCode, 'dbReturn': dbReturn}
    else:
        return {'errorCode': errorCode}


def sanitizeInput(mongoDbInput):
    #global connection
    # print(type(mongoDbInput))
    # for key in mongoDbInput.keys():
    #     print('')
    mongoDbInputSane = {}
    try:
        for item in mongoDbInput.items():
            mongoDbInputSane[item[0]] = str(item[1]).replace('$', '\\$')
        return mongoDbInputSane
    except:
        return mongoDbInput


# print(executeMDb('chats','findChildren',{'nodeIndex':0})['dbReturn'][0])

# ConnectionToDb()

# print(db['chat'].update)