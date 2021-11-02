

# not needed Homebrew will do this
# function setupXcodeCLI() {
#     echo "XCode CLI"
#     REPLY="y"
#     if [ -d "/Library/Developer/CommandLineTools" ]; then
#         echo "... XCode CLI seem to be installed"
#         REPLY="n"
#         read -p "    Do you want to reinstall XCode CLI? (y/n)" -n 1 -r
#         echo
#     fi
#     case "$REPLY" in
#         y|Y) 
#             echo "... Installing XCode CLI"
#             xcode-select --install
#             ;;
#         n|N)
#             echo "... skipping"
#             ;;
#     esac

#     echo "Using XCode $(xcode-select -p)"
# }

# Good idea, but the script already require Dropbox; except I 
# find a way to distribute it differently which might be good
# function installBlockers() {
#     shouldDoOp "Do you want to install Bitwarden and Dropbox"
#     if [ $? -ne 0 ]; then
#         return
#     fi
#     open -a "App Store" "https://apps.apple.com/us/app/bitwarden/id1352778147?mt=12"
#     brew install --appdir "$APP_DIR" dropbox
#     echo "Start Dropbox and sync: ApplicationSupport, Apps, dotfiles, Fonts"
#     read -n 1 -p "Press any key to continue after Bitwarden and Dropbox are configured"
#}
function switchToBash() {
    chsh -s /bin/bash
}

function installUnblockers() {
    read -n 1 -p "Enable Rosetta"
    /usr/sbin/softwareupdate --install-rosetta --agree-to-license
    read -n 1 -p "Install Dropbox and start sync (dotfiles, Apps, ApplicationSupport, Fonts)"
    read -n 1 -p "Install Bitwarden from Mac App Store"
    read -n 1 -p "Ask Dropbox to download the above files"
    read -n 1 -p "Set Dropbox to sync other folders (Dox, family, MagicBriefcase, quick_sync, workspaces)"
}

function setupBrew() {
    echo "Install Homebrew"
    if test ! $(which brew); then
        echo "... Installing homebrew..."
        /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
        if [ "$(uname -m)" = "arm64" ]; then
            BREW_CMD=/opt/homebrew/bin/brew
        else
            BREW_CMD=/usr/local/bin/brew
        fi
        PATH="$($BREW_CMD --prefix)/bin:$PATH"
        echo "... Updated PATH: $PATH"
        export PATH
    else
        echo "... already installed"
    fi
}

function brewInPath() {
    echo "Check Homebrew on path"
    if [ "$(uname -m)" = "arm64" ]; then
        BREW_CMD=/opt/homebrew/bin/brew
    else
        BREW_CMD=/usr/local/bin/brew
    fi
    PATH="$($BREW_CMD --prefix)/bin:$PATH"
    echo "... Updated PATH: $PATH"
    export PATH
}

function setupXCode() {
    shouldDoOp "Do you want to install XCode"
    if [ $? -ne 0 ]; then
        return
    fi
    REPLY="y"
    if [ -d "/Applications/Xcode.app" ]; then
        echo "... XCode seem to be installed"
        REPLY="n"
        read -p "    Do you want to reinstall XCode? (y/n)" -n 1 -r
        echo
    fi
    case "$REPLY" in
        y|Y) 
            echo "... Installing XCode"
            open -a "App Store" "https://apps.apple.com/us/app/xcode/id497799835?mt=12"
            read -n 1 -p "   Press any key to continue after XCode was installed"
            if [ -d "/Applications/XCode.app" ]; then
                echo "... switching Build tools to XCode"
                sudo xcode-select -s /Applications/XCode.app
            fi
            ;;
        n|N)
            echo "... skipping"
            ;;
    esac

    echo "Using XCode $(xcode-select -p)"
}

function installBash() {
    brewInPath
    shouldDoOp "Do you want to install bash"
    if [ $? -ne 0 ]; then
        return
    fi
    command $BREW_CMD install bash
    echo "... configure bash"
    echo "$($BREW_CMD --prefix)/bin/bash" | sudo tee -a /etc/shells
    sudo chsh -s $($BREW_CMD --prefix)/bin/bash
    chsh -s $($BREW_CMD --prefix)/bin/bash
    if [[ ! "$SHELL" =~ "bash" ]]; then
        echo "Terminating script now to restart with bash. Restart Terminal and script"
        exit 20
    fi
}

function installBrews() {
    shouldDoOp "Do you want to install brews"
    if [ $? -ne 0 ]; then
        return
    fi

    PACKAGES=(
        fzf
        git
        mas
        readline
        reattach-to-user-namespace
        ripgrep
        the_silver_searcher
        tmux
    )
    echo "... installing brews"
    command $BREW_CMD install ${PACKAGES[@]}
}


function installOptionalBrews() {
    shouldDoOp "Do you want to install optional brews"
    if [ $? -ne 0 ]; then
        return
    fi
    PACKAGES=(
        yqrashawn/goku/goku
        lua
        nnn
        pyenv
        ruby
        youtube-dl
    )
    for pkg in "${PACKAGES[@]}"; do
        read -n 1 -p "... Install $pkg? (y/n): "
        echo ""
        case "$REPLY" in
            y|Y) $BREW_CMD install $pkg
        esac
    done
}

function installAppsWithBrew() {
    shouldDoOp "Do you want to install apps with brews"
    if [ $? -ne 0 ]; then
        return
    fi
    PACKAGES=(
        atext
        appcleaner
        bettertouchtool
        brave-browser
        firefox
        hammerspoon
        macvim
        qbserve
        steermouse
        omnifocus
        homebrew/cask/transmission
        vlc
        yacreader
    )

    for pkg in "${PACKAGES[@]}"; do
        read -n 1 -p "... Install $pkg? (y/n): "
        echo ""
        case "$REPLY" in
            y|Y) command $BREW_CMD install --appdir "$APP_DIR" $pkg
        esac
    done
    ln -s "$HOME/Dropbox/ApplicationSupport/apps/SteerMouse & CursorSense" "$HOME/Library/Application Support/"
    ln -s "$HOME/Dropbox/ApplicationSupport/dirs/Library/Preferences/org.videolan.vlc" "$HOME/Library/Preferences/"
    mkdir "$HOME/Library/Application Support/YACReader" &>/dev/null
    ln -s "$HOME/Dropbox/ApplicationSupport/apps/YACReader/YACReader" "$HOME/Library/Application Support/YACReader/YACReader"
}

function installAppsManually() {
    echo "Download and install manually"
    URLS=(
        "https://karabiner-elements.pqrs.org"
        "https://www.keyboardmaestro.com/main/"
        "https://tapbots.com/pastebot/"
        "https://vimacapp.com"
        "https://www.jetbrains.com/toolbox-app/"
        "https://betamagic.nl/products/newsexplorer.html"
        "https://kapeli.com/dash"
        "https://www.macbartender.com/Bartender4/"
        "https://obsidian.md"
        "https://www.privateinternetaccess.com/download/mac-vpn"
    )
    for url in "${URLS[@]}"; do
        read -n 1 -p "... Download $url? (y/n): "
        echo ""
        case "$REPLY" in
            y|Y) 
                open "$url"
                read -n 1 -p "    click any key to continue"
                ;;
        esac
    done
    ln -s "$HOME/Library/Mobile Documents/iCloud~md~obsidian/Documents/omuninn" "$HOME/omuninn"
}

function installAppStoreAppsManually() {
    # "https://apps.apple.com/us/app/copied/id1026349850"
    # "https://apps.apple.com/us/app/adguard-for-safari/id1440147259?mt=12"
    echo "Download and install apps from App Store"
    URLS=(
        "https://apps.apple.com/us/app/bumpr/id1166066070?mt=12"
        "https://apps.apple.com/us/app/vimari/id1480933944?mt=12"
        "https://apps.apple.com/us/app/day-one/id1055511498?mt=12"
        "https://apps.apple.com/ro/app/stockfish-chess/id801463932?mt=12"
        "https://apps.apple.com/ro/app/wipr/id1320666476?mt=12"
        "https://apps.apple.com/ro/app/daisydisk/id411643860?mt=12"
        "https://apps.apple.com/ro/app/calca/id635758264?mt=12"
        "https://apps.apple.com/ro/app/reeder-5/id1529448980?mt=12"
        "https://apps.apple.com/ro/app/evernote-web-clipper/id1481669779?mt=12"
        "https://apps.apple.com/ro/app/keyword-search/id1558453954"
    )
    for url in "${URLS[@]}"; do
        read -n 1 -p "... Download $url? (y/n): "
        echo ""
        case "$REPLY" in
            y|Y) 
                open -a "App Store" "$url"
                read -n 1 -p "   click any key to continue"
                ;;
        esac
    done
}

function installAppStoreAppsWithMas() {
    # 1026349850  # Copied
    # 1440147259  # AdGuard for Safari
    APPIDS=(
        1166066070  # Bumpr 
        1480933944  # Vimari
        801463932   # Stockfish
        1055511498  # Day One
        1320666476  # Wipr
        411643860
        635758264
        1481669779
        1558453954
        1529448980
        1289378661
        )
    APPNAMES=(
        "Bumpr"
        "Vimari"
        "Stockfish"
        "Day One"
        "Wipr"
        "DaisyDisk"
        "Calca"
        "Reeder"
        "Twitterrific"
        "Evernote Web Clipper"
        "Keyword Search"
    )
    for app in "${!APPIDS[@]}"; do
        read -n 1 -p "... Install ${APPNAMES[$app]} (${APPIDS[$app]})? (y/n): "
        echo ""
        case "$REPLY" in
            y|Y) mas install ${APPIDS[$app]};;
        esac
    done
}

function installFonts() {
    mkdir -p "$HOME/Downloads/Fonts"
    FILES=(
        20160304-HCo_OperatorMono.zip
        20201230-Input-Font.zip
        20170823-mononoki.zip
        PragmataPro/PragmataPro*.ttf
        20211026-ttf-iosevka-fixed-10.zip
        20211026-ttf-iosevka-fixed-slab-10.zip
        )
    for f in "${FILES[@]}"; do
        cp "$HOME/Dropbox/Fonts/_Monospaced/_favs/${f}" "$HOME/Downloads/Fonts"
    done
}

function configureAppsManually() {
    read -n 1 -p "Set computer name in System Preferences > Sharing"
    read -n 1 -p "Turn on iCloud for Documents and Desktop"
    read -n 1 -p "Activate and configure Keyboard Manager; click any key to continue"
    read -n 1 -p "Configure Vimac and click any key to continue"
    read -n 1 -p "Configure aText cloud sync and click any key to continue"
    read -n 1 -p "Configure BetterTouchTool settings from Desktop file and click any key to continue"
    read -n 1 -p "Configure shortcuts in Pastebot"
    read -n 1 -p "Turn off Close windows when quitting an app in System Preferences > General"
    read -n 1 -p "Turn on Use your Apple Watch in System Preferences > Security & Privacy > General"
    read -n 1 -p "Create cron jobs"
}

# https://macos-defaults.com/
function configureApps() {
    shouldDoOp "Do you want to update default settings for Finder, Safari, Mail, etc"
    if [ $? -ne 0 ]; then
        return
    fi

    # Configure macOS Keyboard
    printf "⚙️ Configure Keyboard...\n"
    defaults write -g NSAutomaticSpellingCorrectionEnabled -bool false
    # double space 
    defaults write -g NSAutomaticPeriodSubstitutionEnabled -bool false
    ## Disable automatic capitalization as it’s annoying when typing code
    defaults write -g NSAutomaticCapitalizationEnabled -bool false
    # 2 dashes to ndash
    defaults write -g NSAutomaticDashSubstitutionEnabled -bool false
    defaults write -g NSAutomaticQuoteSubstitutionEnabled -bool false
    # setup key repeating
    defaults write -g ApplePressAndHoldEnabled -bool false

    printf "⚙️Configure Dock...\n"
    defaults write com.apple.dock autohide -bool true
    defaults write com.apple.dock orientation -string right
    ## Show indicator lights for open applications in the Dock
    defaults write com.apple.dock show-process-indicators -bool true
    ## Speed of hiding the dock
    defaults write com.apple.dock autohide-time-modifier -int 0
    killall Dock

    defaults write com.apple.menuextra.clock DateFormat -string "EEE MMM d HH:mm a"


    # Configure macOS Screen Capture
    printf "⚙️ Save screenshots in PNG format...\n"
    defaults write com.apple.screencapture location -string "${HOME}/Desktop"
    defaults write com.apple.screencapture type -string "png"

    # Configure macOS Finder
    printf "⚙️ Configure Finder...\n"
    defaults write -g AppleShowAllExtensions -bool true

    ## Always open everything in Finder's column view. This is important.
    defaults write com.apple.Finder FXPreferredViewStyle Nlsv

    ## When performing a search, search the current folder by default (the default 'This Mac' is "SCev")
    defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"

    ## Disable the warning when changing a file extension 
    defaults write com.apple.finder FXEnableExtensionChangeWarning -bool false

    ## Show the Path bar, this can also be toggled on the View menu:
    # defaults write com.apple.finder ShowPathbar -bool true 
    ## Show the Status Bar, this can also be toggled on the View menu:
    # defaults write com.apple.finder ShowStatusBar -bool true
    ## Shows whole path in the title bar of the file exporser
    # defaults write com.apple.finder _FXShowPosixPathInTitle -bool true

    ## Enable Text Selection in Quick Look Windows
    defaults write com.apple.finder QLEnableTextSelection -bool true


    printf "⚙️ Configure Mail...\n"
    ## Display emails in threaded mode, sorted by date (oldest at the top)
    defaults write com.apple.mail DraftsViewerAttributes -dict-add "DisplayInThreadedMode" -string "yes"
    defaults write com.apple.mail DraftsViewerAttributes -dict-add "SortedDescending" -string "yes"
    defaults write com.apple.mail DraftsViewerAttributes -dict-add "SortOrder" -string "received-date"


    # Configure macOS Safari
    # https://github.com/kpdecker/dotfiles/blob/master/setup/osx-defaults/safari.sh
    # https://gist.github.com/florianrusch/6fdaa4b281b340c006789a8cf4e8c753
    printf "⚙️ Configure Safari...\n"
    ## Privacy: don’t send search queries to Apple
    defaults write com.apple.Safari UniversalSearchEnabled -bool false
    defaults write com.apple.Safari SuppressSearchSuggestions -bool true
    # Set Safari’s home page to `about:blank` for faster loading
    defaults write com.apple.Safari HomePage -string "about:blank"
    # Allow hitting the Backspace key to go to the previous page in history
    # defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2BackspaceKeyNavigationEnabled -bool true
    defaults write com.apple.Safari AutoOpenSafeDownloads -bool false
    # Press Tab to highlight each item on a web page
    defaults write com.apple.Safari WebKitTabToLinksPreferenceKey -bool true
    defaults write com.apple.Safari com.apple.Safari.ContentPageGroupIdentifier.WebKit2TabsToLinks -bool true
    ## Show the full URL in the address bar (note: this still hides the scheme)
    defaults write com.apple.Safari ShowFullURLInSmartSearchField -bool true
    defaults write com.apple.Safari ShowFavoritesBar -bool false
    # developer tools
    defaults write com.apple.Safari IncludeDevelopMenu -bool true
    defaults write com.apple.Safari IncludeInternalDebugMenu -bool true
    defaults write com.apple.Safari WebKitDeveloperExtrasEnabledPreferenceKey -bool true
    defaults write com.apple.Safari "com.apple.Safari.ContentPageGroupIdentifier.WebKit2DeveloperExtrasEnabled" -bool true
    defaults write -g WebKitDeveloperExtras -bool true
    # spell-checking and auto-correction
    defaults write com.apple.Safari WebAutomaticSpellingCorrectionEnabled -bool false
    defaults write com.apple.Safari WebContinuousSpellCheckingEnabled -bool true

    # Configure macOS Terminal
    printf "⚙️ Configure Terminal...\n"
    defaults write com.apple.terminal "Default Window Settings" -string Novel
    plutil -replace "Window Settings.Basic.columnCount" -integer 160 ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Basic.rowCount" -integer 80 ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Basic.CommandString" -string tmux ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Basic.Bell" -bool FALSE ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Basic.ShowActiveProcessInTableTitle" -bool FALSE ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Basic.ShowActiveProcessInTable" -bool FALSE ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Basic.ShowWindowSettingsNameInTitle" -bool FALSE ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Basic.UseBoldFonts" -bool FALSE ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Basic.UseBrightBold" -bool TRUE ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Basic.useOptionAsMetaKey" -bool TRUE ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Basic.VisualBell" -bool FALSE ~/Library/Preferences/com.apple.Terminal.plist

    plutil -replace "Window Settings.Man Page.columnCount" -integer 160 ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Man Page.rowCount" -integer 80 ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Man Page.CommandString" -string tmux ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Man Page.Bell" -bool FALSE ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Man Page.ShowActiveProcessInTableTitle" -bool FALSE ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Man Page.ShowActiveProcessInTable" -bool FALSE ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Man Page.ShowWindowSettingsNameInTitle" -bool FALSE ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Man Page.UseBoldFonts" -bool FALSE ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Man Page.UseBrightBold" -bool TRUE ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Man Page.useOptionAsMetaKey" -bool TRUE ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Man Page.VisualBell" -bool FALSE ~/Library/Preferences/com.apple.Terminal.plist

    plutil -replace "Window Settings.Novel.columnCount" -integer 160 ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Novel.rowCount" -integer 80 ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Novel.CommandString" -string tmux ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Novel.Bell" -bool FALSE ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Novel.ShowActiveProcessInTableTitle" -bool FALSE ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Novel.ShowActiveProcessInTable" -bool FALSE ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Novel.ShowWindowSettingsNameInTitle" -bool FALSE ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Novel.UseBoldFonts" -bool FALSE ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Novel.UseBrightBold" -bool TRUE ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Novel.useOptionAsMetaKey" -bool TRUE ~/Library/Preferences/com.apple.Terminal.plist
    plutil -replace "Window Settings.Novel.VisualBell" -bool FALSE ~/Library/Preferences/com.apple.Terminal.plist


    # Configure macOS Trackpad
    printf "⚙️ Configure Trackpad...\n"
    defaults write com.apple.driver.AppleBluetoothMultitouch.trackpad Clicking -bool true
    defaults -currentHost write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
    defaults write NSGlobalDomain com.apple.mouse.tapBehavior -int 1
    defaults write NSGlobalDomain com.apple.swipescrolldirection -bool false


    # Configure macOS
    printf "⚙️ Various configuration...\n"
    defaults write com.apple.gamed Disabled -bool true
    # sudo /usr/libexec/ApplicationFirewall/socketfilterfw --setglobalstate on
    defaults write com.apple.TimeMachine DoNotOfferNewDisksForBackup -bool true
}


function setupSdkman() {
    echo "Install SDK"
    if test ! $(which sdk); then
        echo "... Installing SDKMAN..."
        curl -s "https://get.sdkman.io" | bash
        source "$HOME/.sdkman/bin/sdkman-init.sh"
        sdk version
    else
        echo "... already installed"
    fi
}

function shouldDoOp() {
    read -n 1 -p "$1 (y/n)? "
    echo ""
    case "$REPLY" in
        y|Y) return 0;;
        *) return 1;;
    esac
}


read -p "Set APP_DIR (default /Applications):" APP_DIR
if [ -z "$APP_DIR" ]; then
    APP_DIR="/Applications"
fi
BREW_CMD="brew"
echo $APP_DIR
# setupXcodeCLI
# setupXCode
# setupBrew
# installBash
# installBrews
# installOptionalBrews
# installAppsWithBrew
# installAppsManually
# installAppStoreAppsManually
# installAppStoreAppsWithMas
# configureAppsManually
# configureApps
# installFonts
setupSdkman

