#!/bin/bash
cd "$(dirname "${BASH_SOURCE}")"
git pull origin master
function status() {
    IFS=$'\n'
    for file in $(ls -A) ; do
        if [ $file = "bootstrap.sh" ]; then
            continue
        fi
        if [ $file = "README.md" ]; then
            continue
        fi
        if [ $file = ".git" ]; then
            continue
        fi
        echo "Comparing $file"
        if [ -d $file ]; then
            diff -r $file $HOME/$file
        else
            diff $file $HOME/$file
        fi
    done
}
function doIt() {
	rsync --exclude ".git/" --exclude ".DS_Store" --exclude "bootstrap.sh" --exclude "README.md" -av . ~
}
case "$1" in
    --status)
        status
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
unset status
source ~/.bash_profile
