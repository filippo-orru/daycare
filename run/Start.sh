tmux ls | grep -sq daycare && tmux kill-session -t "daycare"
tmux -2u new-session -d -s "daycare" -n "flask" "source env/bin/activate && run/Mdb.sh && while true; do run/Flask.sh; echo; read -p \" >> Enter to restart!\"; done;"
tmux new-window -t "daycare" -n "elm" "run/Elm.sh; read -p \">>Press enter to leave\""
tmux att -t "daycare"