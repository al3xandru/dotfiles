#!/bin/bash
cd "$(dirname "${BASH_SOURCE}")"
git pull origin master
function doStatus() {
    IFS=$'\n'
    for file in $(ls -A) ; do
        if [ $file = ".git" ]; then
            continue
        fi
        if [ $file = ".gitmodules" ]; then
            continue
        fi
        if [ $file = ".DS_Store" ]; then
            continue
        fi
        if [ $file = "bootstrap.sh" ]; then
            continue
        fi
        if [ $file = "README.md" ]; then
            continue
        fi
        echo "Comparing $file with $HOME/$file"
        if [ -d $file ]; then
            diff -r $file $HOME/$file
        else
            diff $file $HOME/$file
        fi
    done
}
function doIt() {
	rsync --exclude ".git/" --exclude ".DS_Store" --exclude "bootstrap.sh" --exclude "README.md" -av . $HOME
    source ~/.bash_profile
}
case "$1" in
    --status)
        doStatus
        ;;
    --force)
        doIt
        ;;
    -f)
        doIt
        ;;
    *)
	    read -p "This may overwrite existing files in your home directory. Are you sure? (y/n) " -n 1
	    echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            doIt
        fi
        ;;
esac
unset doIt
unset doStatus
