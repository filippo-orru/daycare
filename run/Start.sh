tmux -2u new-session -d -s "daycare" -n "flask" "source env/bin/activate && run/Mdb.sh && run/Flask.sh; read -p \">>Press enter to leave\""
tmux new-window -t "daycare" -n "elm" "run/Elm.sh; read -p \">>Press enter to leave\""
tmux att -t "daycare"