#!/bin/bash
dir=$1
if [ -z ${dir} ]; then
   dir="$HOME/Dropbox/workspace"
elif [ ! -d ${dir} ]; then
  dir="$HOME/Dropbox/workspace"
fi
echo "Using working dir: ${dir}"
sessname=$(basename $dir)
tmux -2 new-session -d -s "$sessname"
tmux rename-window "main"
tmux split-window -h
tmux select-pane -t 1
tmux send-keys "cd $dir" C-m
tmux select-pane -t 2
tmux send-keys "cd $dir" C-m
tmux split-window -v
tmux select-pane -t 3
tmux send-keys "cd $dir" C-m
tmux new-window -n "vim"
tmux send-keys "cd $dir" C-m
tmux new-window -n "vim active | nvall"
tmux split-window -h
tmux select-pane -t 1
tmux send-keys "cd $HOME/Dropbox/Dox/active" C-m
tmux select-pane -t 2
tmux send-keys "cd $HOME/Dropbox/Dox/nvall" C-m

tmux select-window -t "main"
tmux -2 attach-session -t "$sessname"
