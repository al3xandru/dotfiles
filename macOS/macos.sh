#!/bin/bash
if [ -z $(which brew) ]; then
    echo "Installing Homebrew (http://brew.sh)"
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
fi
echo "Enable Cask (https://caskroom.github.io)"
brew tap caskroom/cask

BREWS=("aspell" "git" "go" "lua" "openssl" \
       "ranger" "readline" "the_silver_searcher" "tmux" "tree")
function brewInstall() {
    for pkg in ${BREWS[@]}; do
        echo "brew install $pkg"
    done
    echo "brew tap universal-ctags/universal-ctags"
    echo "brew install --HEAD universal-ctags"
}

function installPyenv() {
    if [ -z $(which git) ]; then
        echo "pyenv install requires Git"
        return
    fi
    # git clone https://github.com/yyuu/pyenv.git ~/.pyenv
    # git clone https://github.com/yyuu/pyenv-virtualenv.git ~/.pyenv/plugins/pyenv-virtualenv
    # git clone git://github.com/yyuu/pyenv-pip-migrate.git ~/.pyenv/plugins/pyenv-pip-migrate
    pyenv install --list
    read -p "Enter Python distro to install:" pyver
    echo "pyenv install $pyver"
    echo "pyenv global $pyver"
}

function installVim() {
    if [ -z $(which pyenv) ]; then
        echo "vim install requires pyenv"
        return
    fi
    echo "brew install --build-from-source --with-custom-python --with-lua vim"
}

# separate in essentials and optionals
# add: Evernote, Vox
APPS=("1password" "alfred" "bettertouchtool" "duet" "macvim" \
      "mattr-slate" "hazel" "resilio-sync" "little-snitch" "appcleaner" \
      "bartender" "dash" "google-chrome" "itsycal" "keyboard-maestro" \
      "mailplane" "omnifocus" "taskpaper" "spotify" "vlc" \
      "intellij-idea" "pycharm")

function caskInstall() {
    # brew cask install dropbox
    read -p "Press [enter] to continue installation once you configured Dropbox"
    for app in ${APPS[@]}; do
        echo "brew cask install ${app}"
    done
}

echo "Installing Homebrew packages"
brewInstall

echo "Installing pyenv"
installPyenv

# Install applications
echo "Installing applications"
caskInstall

# brew cask install alternote
# brew cask install evernote
#brew cask install keyboard-maestro

