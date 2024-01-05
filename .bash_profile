# 
# Bash setup inspired by: https://github.com/mathiasbynens/dotfiles
# https://scriptingosx.com/2017/04/about-bash_profile-and-bashrc-on-macos/
#

[ -r ~/.precfg ] && [ -f ~/.precfg ] && source ~/.precfg
[ -r ~/.path ] && [ -f ~/.path ] && source ~/.path
[ -r ~/.aliases ] && [ -f ~/.aliases ] && source ~/.aliases
[ -r ~/.exports ] && [ -f ~/.exports ] && source ~/.exports
[ -r ~/.functions ] && [ -f ~/.functions ] && source ~/.functions

[ -r ~/.bashrc ] && source ~/.bashrc

#THIS MUST BE AT THE END OF THE FILE FOR SDKMAN TO WORK!!!
export SDKMAN_DIR="/Users/alex/.sdkman"
[[ -s "/Users/alex/.sdkman/bin/sdkman-init.sh" ]] && source "/Users/alex/.sdkman/bin/sdkman-init.sh"
