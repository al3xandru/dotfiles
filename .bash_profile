# 
# Bash setup inspired by: https://github.com/mathiasbynens/dotfiles
# https://scriptingosx.com/2017/04/about-bash_profile-and-bashrc-on-macos/
#

# Load the shell dotfiles, and then some:
# * ~/.precfg can be used locally to enable/disable specific features
# * ~/.postcfg can be used for other setttings you don't want to commit
# for file in $HOME/.{precfg,path,aliases,exports,functions}; do
#     [ -r "$file" ] && [ -f "$file" ] && source "$file"
# done
# unset file

[ -r ~/.precfg ] && [ -f ~/.precfg ] && source ~/.precfg
[ -r ~/.path ] && [ -f ~/.path ] && source ~/.path
[ -r ~/.aliases ] && [ -f ~/.aliases ] && source ~/.aliases
[ -r ~/.exports ] && [ -f ~/.exports ] && source ~/.exports
[ -r ~/.functions ] && [ -f ~/.functions ] && source ~/.functions

[ -r ~/.bashrc ] && source ~/.bashrc

### MANAGED BY RANCHER DESKTOP START (DO NOT EDIT)
export PATH="/Users/alexandp/.rd/bin:$PATH"
### MANAGED BY RANCHER DESKTOP END (DO NOT EDIT)
