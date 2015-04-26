#!/bin/bash
cd "$(dirname "${BASH_SOURCE}")"

function doStatus() {
    echo "Status of the target directory:"
    echo ""
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
            diff -rq $file $HOME/$file | grep -v -e 'tags' -e 'netrwhist' -e 'README.md'
        else
            diff -q $file $HOME/$file
        fi
    done
}
function doIt() {
    rsync --exclude ".git/" --exclude ".DS_Store" --exclude "bootstrap.sh" --exclude "README.md" -av . $HOME
    source ~/.bash_profile
}
case "$1" in
    -d)
        read -p "This may overwrite existing files in your home directory. Are you sure? (y/n) " -n 1
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            doIt
        fi
        ;;

    --do)
        read -p "This may overwrite existing files in your home directory. Are you sure? (y/n) " -n 1
        echo
        if [[ $REPLY =~ ^[Yy]$ ]]; then
            doIt
        fi
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
    -s)
        doStatus
        ;;
    --status)
        doStatus
        ;;
    --help)
        echo "bootstrap.sh installs .dotfiles in your home dir"
        echo ""
        echo -e "\t-d --do\t\t\tAsks for confirmation before proceeding"
        echo -e "\t-f --force\t\tProceeds with no confirmation"
        echo -e "\t-u --update\t\tFirstly performs a git pull and then proceeds to upgrade"
        echo -e "\t-s --status\t\tPerforms a diff and displays results"
        echo -e "\t--help\t\t\tDisplays this help"
        echo ""
        ;;
    *)
        doStatus
        ;;
esac
unset doIt
unset doStatus
