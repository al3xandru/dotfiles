# 
# Bash setup inspired by: https://github.com/mathiasbynens/dotfiles
#
ostype=`uname`
case "$ostype" in
    "Darwin")
        export _OSTYPE="Mac"
        ;;
    "Linux")
        export _OSTYPE="Lin"
        ;;
esac
unset ostype
# Load the shell dotfiles, and then some:
# * ~/.precfg can be used locally to enable/disable specific features
# * ~/.path can be used to extend `$PATH`
# * ~/.postcfg can be used for other setttings you don't want to commit
for file in ~/.{precfg,exports,path,prompt_bash,aliases,functions,postcfg}; do
    [ -r "$file" ] && [ -f "$file" ] && source "$file"
done
unset file
unset _OSTYPE

shopt -s histappend

# Add tab completion for SSH hostnames based on ~/.ssh/config, ignoring wildcards
[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2 | tr ' ' '\n')" scp sftp ssh

[ -f /usr/local/etc/bash_completion.d/git-completion.bash ] && source /usr/local/etc/bash_completion.d/git-completion.bash

# if possible, add tab completion for many more commands
[ -f /etc/bash_completion ] && source /etc/bash_completion
