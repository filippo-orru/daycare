from requests import session
from datetime import datetime, timedelta
import sys, os.path, time, re, json
from collections import namedtuple

secondsBetweenRequests = 2
logTime = True
printLog = False


def get(_getPage,
        _postPage=None,
        payload=None,
        verify=True,
        useLocalPages=True,
        c=None):
    # local setup
    localPath = 'local/{}.local'.format(urlToFilename(_getPage))
    if not os.path.exists('local/'):  # create path if not exsit
        os.makedirs('local/')

    localCopy = localCopyExists(localPath)  # false if not exist
    if useLocalPages and localCopy:
        print('Using local file ' + localPath)
        return localCopy

    else:  # need to download page
        print('Downloading ' + _getPage)
        if verify == True:
            import urllib3
            urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)
        if c == None or c.cookies == None or c.cookies == '{}':
            c = session()
            if payload != None and _postPage != None:  # only post if payload
                print('Doing post')
                limitRequestTime()
                c.post(_postPage, data=payload)
        else:
            print('Skipping post')
            pass
            # uprint('post is ' + postresponse.text)
            #, verify=verify)
        print('Doing get')  # get in any case
        limitRequestTime()
        response = c.get(_getPage, verify=verify)
        # writeTo('test.html', postresponse.text, 'w')
        if response.encoding is None or response.encoding == 'ISO-8859-1':
            response.encoding = response.apparent_encoding

        writeTo(localPath, response.text, 'w')
        print('Saved to disk ' + localPath)
        return [response.text, c]
    return False


def postAndGet2(_postPage, _getPage, payload, verify=True, useLocalPages=True):
    localPath = 'local/{}.local'.format(urlToFilename(_getPage))
    if not os.path.exists('local/'):
        os.makedirs('local/')
    localCopy = readFrom(localPath)
    if not useLocalPages:
        localCopy = False
    if localCopy:
        print('Using local file ' + localPath)
        return localCopy
    else:
        print('Downloading ' + _getPage)
        limitRequestTime()
        with session() as c:
            c.post(_postPage, data=payload, verify=verify)
            response = c.get(_getPage, verify=verify)

            if response.encoding is None or response.encoding == 'ISO-8859-1':
                response.encoding = response.apparent_encoding
        # if useLocalPages == False and readFrom(localPath) == response.text:
        #     print('suggest using local pages')
        #     useLocalPages == True
        writeTo(localPath, response.text, 'w')
        print('Saved to disk ' + localPath)
        return response.text
    return False


def uprint(*objects, sep=' ', end='\n', file=sys.stdout):
    enc = file.encoding
    if enc == 'UTF-8':
        print(*objects, sep=sep, end=end, file=file)
    else:
        f = lambda obj: str(obj).encode(enc, errors='backslashreplace').decode(enc)
        print(*map(f, objects), sep=sep, end=end, file=file)


def log(text, _type='Info'):
    global printLog, logTime
    if printLog:
        if logTime:
            uprint('[{}] ({}): {}'.format(
                _type,
                datetime.now().strftime('%Y-%m-%d %H:%M:%S'), text))
        else:
            uprint('[{}]: {}'.format(_type, text))


def fileExists(path):
    return os.path.exists(path)


def readFrom(path):
    # print('read from path ' + str(path))
    try:
        if os.path.exists(path):
            # time = str(datetime.now())[:len(str(datetime.now())) - 4]
            fhh = open(path, encoding='utf-8')
            content = fhh.read()
            fhh.close()
            return content
    except:
        return False

    return False


def writeTo(path, content, append='a'):
    # print('write to path ' + str(path))
    # path = urlToFilename(path, fileExtension)
    if not os.path.exists(path):
        # if not '.' in path:
        # print('making folders')
        #folderPath = path[path.find('/')[len(path.find('/') - 1)]:]
        # print('folderpath: ' + folderPath)
        # os.makedirs(path)
        # else:
        # print('not making folders')
        # if not os.path.exists(path):
        fhh = open(path, 'w', encoding='utf-8')
        fhh.write('')
        fhh.close()
    if os.path.exists(path):
        fhh = open(path, append, encoding='utf-8')
        fhh.write(content)
        fhh.close()
        return True

    return False


def localCopyExists(path):
    if os.path.exists(path):
        return readFrom(path)
    else:
        return False


def urlToFilename(url):
    withOutHttp = re.sub(r"(?:https?:\/\/)", "", url, 0, re.MULTILINE)
    if url == withOutHttp:
        return url
    else:
        noSymbols = re.sub(r"(\W)", "-", withOutHttp, 0)
        return noSymbols


def limitRequestTime():
    global secondsBetweenRequests
    enoughTimeBetweenRequests = False

    while not enoughTimeBetweenRequests:
        nowtime = datetime.now()
        lastAccessRaw = readFrom('lastAccess.txt')
        try:
            lastAccess = datetime.strptime(lastAccessRaw,
                                           '%Y-%m-%d %H:%M:%S.%f')
        except ValueError:
            break
        nextRequestAt = lastAccess + timedelta(0, secondsBetweenRequests)

        if nowtime > nextRequestAt:
            # print('enoughTimeBetweenRequests')
            enoughTimeBetweenRequests = True
        else:
            # print('not enoughTimeBetweenRequests')
            enoughTimeBetweenRequests = False
            time.sleep(0.1)

    writeTo('lastAccess.txt', str(nowtime), 'w')
    return True


def cleanQeuery(query):
    if len(query) > 2 and type(query) == str:
        query = re.sub(r"^(?: )?(.*?)(?: )?$", "\\1", query, 0, re.MULTILINE)

    return query


def cleanList(*uinput):
    '''
    Removes any empty or None items from the list
    '''
    return list(filter(None, uinput))


if __name__ == "__main__":
    pass
    # data = '[{"_id": ObjectId("5c632d5d3ee0872e8571d9d5"), "name": "Führerschein", "description": "123 Ich will meinen Führerschein endlich machen", "timelimit": datetime.datetime(2019, 7, 24, 22, 0)}]'
