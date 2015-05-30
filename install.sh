#!/bin/bash
cd "$(dirname ${BASH_SOURCE})"

function doStatus() {
    echo "1) Checking symlinks are in place"
    echo ""
    for f in .{aliases,exports,functions,path,prompt_bash,slate}; do
        if [ ! -h $HOME/"${f}" ]; then
            echo "~/${f} is NOT a symlink"
        fi
    done
    if [ ! -h "$HOME/.vimrc" ]; then
        echo "Vim ~/.vimrc is NOT a symblink"
    fi
    if [ ! -h "$HOME/.emacs.d/init.el" ]; then
        echo "Emacs ~/.emacs.d/init.el is NOT a symlink"
    fi


    arr=("." ".git" ".gitignore" ".gitmodules" ".rsyncexclude" "README.md" "bootstrap.sh" "install.sh" ".DS_Store")
    
    echo ""
    echo "2) Comparing dirs"
    for file in $(find . -type d -maxdepth 1 | sed 's/^\.\///'); do
        if [[ " ${arr[@]} " =~ " ${file} " ]]; then
            continue
        fi
        echo ""
        echo "Comparing $file with $HOME/$file"
        diff -rq $file $HOME/$file | grep -v -e 'tags' -e 'netrwhist' -e 'README.md'
    done
    echo ""
    echo "3) Comparing files"
    echo ""
    for file in $(find . -type f -maxdepth 1 | sed 's/^\.\///'); do
        if [[ " ${arr[@]} " =~ " ${file} " ]]; then
            continue
        fi
        echo "Comparing $file with $HOME/$file"
        diff -q $file $HOME/$file
    done
}

function doInstall() {
    rsync --exclude-from=.rsyncexclude -ai . $HOME

    for f in .{aliases,exports,functions,path,prompt_bash,slate}; do
        proc $f
    done

    proc ".vimrc"
    proc ".emacs.d/init.el"

    #for d in {.vim,.emacs.d,.virtualenv}; do
        #if [ "$d" = ".emacs.d" ]; then
            ## add -q if too verbose
            #rsync --exclude ".DS_Store" --exclude "init.el" -ai $d $HOME
        #else
            #rsync --exclude ".DS_Store" -ai ${d} $HOME
        #fi
    #done

    #exc=("." ".aliases" ".export" ".functions" ".path" ".prompt_bash")
    #echo "TODO: copy the rest of the files"
}

function proc() {
    f="$1"
    hf=$HOME/"$1"

    if [ -a "${hf}" ]; then
        diff -sq "${f}" "${hf}" &> /dev/null

        if [ 0 -ne $? ]; then
            echo "File ${f} is different!"
        else
            if [ -h "${hf}" ]; then
                echo "Target ${f} is a symlink to $(ls -l ${hf} | awk '{print $11}')"
            else
                echo "Target file ${f} exists, it's the same, but not a symlink"
                rm -f "${hf}"
                ln -s "${PWD}/${f}" "${hf}"
                echo "Target filed removed and symlink created"
            fi
        fi
    else
        echo "Target symlink for ${f} doesn't exist"
        ln -s "${PWD}/${f}" "${hf}"
    fi
}

case "$1" in
    -i)
        #read -p "This may overwrite existing files in your home directory. Are you sure? (y/n) " -n 1
        #echo
        #if [[ $REPLY =~ ^[Yy]$ ]]; then
            #doInstall
        #fi
        doInstall
        ;;

    --install)
        #read -p "This may overwrite existing files in your home directory. Are you sure? (y/n) " -n 1
        #echo
        #if [[ $REPLY =~ ^[Yy]$ ]]; then
            #doInstall
        #fi
        doInstall
        ;;
    -s)
        doStatus
        ;;
    --status)
        doStatus
        ;;
    --help)
        echo "install.sh installs symlinks to .dotfiles in your home dir"
        echo ""
        echo -e "\t-i --install\t\tInstall symlinks to dotfiles in the home dir"
        echo -e "\t-s --status\t\tPerforms a diff and displays results"
        echo -e "\t--help\t\t\tDisplays this help"
        echo ""
        ;;
    *)
        doStatus
        ;;
esac
unset doInstall
unset doStatus
