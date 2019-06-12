if ! tmux ls | grep -sq daycare; then
    tmux -2u new-session -d -s "daycare" -n "flask" "source env/bin/activate && run/Mdb.sh && while true; do run/Flask.sh; echo; read -p \" >> Enter to restart! (Ctrl+C to quit)\"; done;"
    tmux new-window -t "daycare" -n "elm" "run/Elm.sh; read -p \">>Press enter to leave\""
fi
tmux att -t "daycare"