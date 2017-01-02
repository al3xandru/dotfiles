# macOS clean install

1.  Download Dropbox
2.  Sync main Dropbox folders
    1.  1Password
    2.  ApplicationSupport
    3.  Apps
    4.  workspace/miscellaneous/dotfiles
    5.  Dox
    6.  TextExpander (?)
3.  Install 1Password (from App Store)
4.  Add .ssh (stored in 1Password)
    - [ ] add it to 1Password

## Ensure symlinks

See `user_symlinks.sh`

## Cron

Cron backups are available in Dropbox

## Applications

1. Automated installation through `macos.sh`
    1. homebrew
        1. git
    2. pyenv
    3. some applications
2. Install dotfiles
    1. Make sure `git submodule init`

### Applications list

What applications do I want installed?

1. Yes
    1. BetterTouchTool ✔
    2. Slate ✔
    3. Bartender 2 ✔
    4. Hazel ✔
    5. Resilio Sync ✔
    6. LittleSnitch ✔
    7. Itsycal ✔
    8. Private Internet Access ✔
    9. Keyboard Maestro ✔
2. Maybe
    1. Alfred ✔
    2. nvALT
    3. Hocus Focus
    5. Timing
    6. PopClip
    7. TextExpander
    8. Irvue ✔
    9. Boom 2 ✔
    10. iTerm
3. Might be needed
    1. Logitech Unifying Software
    2. Banktivity 5
    3. BBEdit
    4. DaisyDisk
    5. Dash ✔
    6. Day One ✔
    7. duet ✔
    8. Emacs (?)
    9. Google Chrome ✔
    10. iA Writer
    11. IntelliJ ✔
    12. Mailplane 3 ✔
    13. Marked 2
    14. MPlayerX ✔
    15. OmniFocus ✔
    16. PyCharm ✔
    17. Scapple
    18. Slack ✔
    19. Skype
    20. Spotify ✔
    21. TaskPaper ✔
    22. Textual
    23. Tweetbot ✔
    24. Ulysses
    25. uTorrent
    26. YACReader/YACReaderLibrary
4.  Dropped in 2016
    1.  TextExpander
    2.  PopClip
    3.  RescueTime

## Safari extensions

- no automation here
- 

Todos

- [x] Find a safe way to store `.ssh`
- [x] Before installing applications find a way to fix symlinks
- [x] Automate installation of must haves (https://caskroom.github.io)



