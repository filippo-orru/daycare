import configparser, os
hasParsed = False
configuraition = None


def parse():
    global hasParsed
    global configuraition

    if not hasParsed:
        if 'flask-flaskSecretKey' in os.environ:
            if os.environ['flask-flaskSecretKey'] != '' and os.environ['flask-flaskSecretKey'] != None:
                return readAndStoreOsVar(
                )  # if running on server without config file
        else:
            return readAndStoreConfig()
    return configuraition


def readAndStoreConfig():
    config = configparser.ConfigParser()
    config.read('databaseConfig.ini')
    # print()
    return config


def readAndStoreOsVar():
    #envVar = {}
    envVar = {
        'flask': {
            'flaskSecretKey': os.environ['flask-flaskSecretKey']
        },
        'mlab': {
            'clientUrl': os.environ['mlab-clientUrl'],
            'username': os.environ['mlab-username'],
            'password': os.environ['mlab-password']
        }
    }
    return envVar


#print(parse()['mlab']['username'])
# print(parse()['mongodb']['clientUrl'])
