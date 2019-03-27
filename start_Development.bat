REM start "" "W:\Filippo\Development\_Git\daycare\runFlask.bat"
REM start "" "vscode://file/W:/Filippo/Development/_Git/daycare/"
REM start "" "C:\Program Files (x86)\Google\Chrome\Application\chrome.exe" "http://127.0.0.1:8000/daycare.elm"

start "" "W:\Filippo\Development\_Git\daycare\runMdb.bat"
start "" "W:\Filippo\Development\_Git\daycare\runElm.bat"
timeout 1
start "" "C:\Users\fefe\AppData\Local\MongoDBCompassCommunity\MongoDBCompassCommunity.exe"
start "" "C:\Users\fefe\AppData\Local\Postman\Update.exe" --processStart "Postman.exe"
start "" "http://127.0.0.1:8000/daycare.elm"
start "" "W:\Filippo\Development\_Git\daycare\server\Schemes\export\apiScheme.html"
start "" "W:/Filippo/Development/_Git/daycare.code-workspace"
exit