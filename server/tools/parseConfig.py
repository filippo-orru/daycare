import configparser, os
hasParsed = False
configuraition = None


def parse(path=None):
    global hasParsed
    global configuraition

    if not hasParsed:
        if 'flask-flaskSecretKey' in os.environ:
            if os.environ['flask-flaskSecretKey'] != '' and os.environ['flask-flaskSecretKey'] != None:
                return readAndStoreOsVar(
                )  # if running on server without config file
        else:
            if path:
                return readAndStoreConfig(path)
            else:
                raise "no path supplied"
    return configuraition


def readAndStoreConfig(path):
    config = configparser.ConfigParser()
    config.read(path)
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
