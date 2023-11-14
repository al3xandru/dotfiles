# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# disable XON/XOFF flow control
# I don't recall what's this for
stty -ixon
[ -r ~/.postcfg ] && [ -f ~/.postcfg ] && source ~/.postcfg

# if possible, add tab completion for many more commands
[ -f /etc/bash_completion ] && source /etc/bash_completion
[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"

shopt -s nullglob
for file in /usr/local/etc/bash_completion.d/*; do
    [ -f "$file" ] && source "$file"
done
[ -f ~/.fzf.bash ] && source ~/.fzf.bash
shopt -u nullglob

eval "$(starship init bash)"
eval "$(zoxide init bash)"

### MANAGED BY RANCHER DESKTOP START (DO NOT EDIT)
export PATH="/Users/alexandp/.rd/bin:$PATH"
### MANAGED BY RANCHER DESKTOP END (DO NOT EDIT)
