#!/bin/bash

function doList() {
    cd $HOME
    find . -not \( \
        -path ./.marks -prune -o \
        -path ./.pyenv -prune -o \
        -path ./.rbenv -prune -o \
        -path ./Downloads -prune -o \
        -path ./Dropbox/ApplicationSupport/atom -prune -o \
        -path ./Dropbox/workspace -prune -o \
        -path ./Library/Application\ Support/Audio\ Hijack -prune -o \
        -path ./Library/Application\ Support/iCloud -prune -o \
        -path ./Library/Application\ Support/WebEx\ Folder -prune -o \
        -path ./Library/Caches -prune -o \
        -path ./Library/CoreData -prune -o \
        -path ./Library/Containers -prune -o \
        -path ./Library/Developer -prune -o \
        -path ./Library/Frameworks -prune -o \
        -path ./Library/Group\ Containers -prune -o \
        -path ./Library/Internet\ Plug-Ins -prune -o \
        -path ./Library/Logs -prune -o \
        -path ./Library/Mobile\ Documents -prune -o \
        -path ./Library/PreferencePanes -prune -o \
        -path ./Library/Saved\ Application\ State -prune -o \
        -path ./.Trash -prune \
        \) -type l -print
    cd -
}

function doHelp() {
    echo "$0 can either list or install symlinks"
    echo ""
    echo -e "\t-i --install\t\tInstall symlinks"
    echo -e "\t-l --list\t\tList existing symlinks"
    echo -e "\t--help\t\t\tDisplays this help"
    echo ""
}

function doWhich() {
    src_dir="./Library/Application Support"
    tgt_dir="./Dropbox/ApplicationSupport"
    symlinks=("BBEdit" \
        "EagleFiler/Capture Scripts" \
        "EagleFiler/Stationary" \
        "LaunchBar/Actions" \
        "LaunchBar/Calculator" \
        "nvALT" \
        "TaskPaper")
    for f in ${symlinks[@]}; do
        if [ -f "$HOME/$src_dir/$f" ]; then
            echo "[error] File found instead of symlink: $HOME/$src_dir/$f"
        elif [ -h "$HOME/$src_dir/$f" ]; then
            echo "[ warn] Symlink already in place: $HOME/$src_dir/$f"
        elif [ -d "$HOME/$src_dir/$f" ]; then
            echo "[error] Dir found instead of symlink: $HOME/$src_dir/$f"
        else
            echo "ln -s \"$HOME/$tgt_dir/$f\" \"$HOME/$src_dir/$f\""
        fi
    done

    symlinks=("IntelliJIdea2016.3" "PyCharm2016.3")
    for f in ${symlinks[@]}; do
        src_dir="$HOME/Library/Application Support/$f"
        if [ -f "$src_dir" ]; then
            echo "[error] File found instead of symlink: $src_dir"
        elif [ -h "$src_dir" ]; then
            echo "[ warn] Symlink already in place: $HOME/$src_dir/$f"
        elif [ -d "$HOME/$src_dir/$f" ]; then
            echo "[error] Dir found instead of symlink: $HOME/$src_dir/$f"
        else
            echo "ln -s \"$HOME/Dropbox/ApplicationSupport/$f\" \"$src_dir\""
        fi
        src_dir="$HOME/Library/Preferences/$f"
        if [ -f "$src_dir" ]; then
            echo "[error] File found instead of symlink: $src_dir"
        elif [ -h "$src_dir" ]; then
            echo "[ warn] Symlink already in place: $src_dir"
        elif [ -d "$HOME/$src_dir/$f" ]; then
            echo "[error] Dir found instead of symlink: $src_dir"
        else
            echo "ln -s \"$HOME/Dropbox/ApplicationSupport/Library/Preferences/$f\" \"$src_dir\""
        fi
    done
}

case "$1" in
    -l)
        doList
        ;;

    --list)
        doList
        ;;
    -i)
        doInstall
        ;;
    --install)
        doInstall
        ;;
    -s)
        doWhich
        ;;
    --help)
        doHelp
        ;;
    *)
        doHelp
        ;;
esac
unset doHelp
# unset doStatus
