# 
# Bash setup inspired by: https://github.com/mathiasbynens/dotfiles
#
# Load the shell dotfiles, and then some:
# * ~/.precfg can be used locally to enable/disable specific features
# * ~/.path can be used to extend `$PATH`
# * ~/.postcfg can be used for other setttings you don't want to commit
for file in ~/.{precfg,exports,path,prompt_bash,aliases,functions,postcfg}; do
    [ -r "$file" ] && source "$file"
done
unset file

shopt -s histappend

# Add tab completion for SSH hostnames based on ~/.ssh/config, ignoring wildcards
[ -e "$HOME/.ssh/config" ] && complete -o "default" -o "nospace" -W "$(grep "^Host" ~/.ssh/config | grep -v "[?*]" | cut -d " " -f2 | tr ' ' '\n')" scp sftp ssh

# if possible, add tab completion for many more commands
[ -f /etc/bash_completion ] && source /etc/bash_completion

# add tab completion for jump (alias cj)
_completejump() {
    local curw=${COMP_WORDS[COMP_CWORD]}
    #local wordlist=$(find $MARKPATH -type l | awk -F '/' '{print $NF}')
    #COMREPLY=($(compgen -W '${wordlist[@]}' -- "$curw"))
    COMPREPLY=( $(compgen -W "$( ls $MARKPATH )" -- $curw) )
    return 0
}

complete -o "default" -o "nospace" -F _completejump cj unmark jump
