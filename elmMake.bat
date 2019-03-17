w:
cd W:\Filippo\Development\_Git\daycare\client
elm make daycare.elm --output elm.js > nul
cd ..\
move client\elm.js server\frontend\static\elm.js