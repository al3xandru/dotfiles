#!/bin/bash
cd "$(dirname "${BASH_SOURCE}")"

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
            diff -r $file $HOME/$file | grep -v -e 'tags' -e 'netrwhist' -e 'README.md'
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
    --update)
        git pull origin master
        read -p "This may overwrite existing files in your home directory. Are you sure? (y/n) " -n 1
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            doIt
        fi
        ;;
    -u)        
        git pull origin master
        read -p "This may overwrite existing files in your home directory. Are you sure? (y/n) " -n 1
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            doIt
        fi
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
