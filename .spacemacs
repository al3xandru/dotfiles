;; -*- mode: dotspacemacs -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; UI
     eyebrowse
     themes-megapack
     theming
     ;; docs
     dash
     search-engine
     ;; git
     git
     ;; completion
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup t)
     ;; cscope
     semantic
     (syntax-checking :variables
                      syntax-checking-enable-by-default nil
                      syntax-checking-enable-tooltips nil)
     ;; languages
     asciidoc
     c-c++
     csharp
     emacs-lisp
     go
     html
     java
     javascript
     markdown
     org
     php
     python
     ruby
     scala
     shell-scripts
     swift
     yaml
     ;; vim
     evil-commentary
     evil-snipe
     ;; misc
     imenu-list
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(
                                      evil-vimish-fold)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   ;; Either `vim' or `emacs'. Evil is always enabled but if the variable
   ;; is `emacs' then the `holy-mode' is enabled at startup.
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progess in `*Messages*' buffer.
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to a .PNG file.
   ;; If the value is nil then no banner is displayed.
   ;; dotspacemacs-startup-banner 'official
   dotspacemacs-startup-banner nil
   ;; t if you always want to see the changelog at startup
   dotspacemacs-always-show-changelog t
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'."
   dotspacemacs-startup-lists '(recents projects bookmarks)
   dotspacemacs-startup-recent-list-size 10
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(alect-light
                         misterioso
                         ample
                         alect-dark
                         alect-dark-alt
                         heroku
                         material
                         solarized-light
                         solarized-dark
                         leuven
                         gruvbox
                         monokai
                         zenburn
                         flatui)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Operator Mono"
                               :size 12
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it.
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil the paste micro-state is enabled. While enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state t
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-guide-key-delay 0.4
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up.
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX."
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (Emacs 24.4+ only)
   dotspacemacs-line-numbers t
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 40
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen.
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   dotspacemacs-smartparens-strict-mode nil
   ;; If non nil advises quit functions to keep server open when quitting.
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "grep" "ack")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now.
   dotspacemacs-default-package-repository nil
   )
  ;; User initialization goes here
  )


(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put almost
any user code here.  The exception is org related code, which should be placed
in `dotspacemacs/user-config'."
  (setq theming-modifications
        '((alect-light (powerline-active1 :background "#ffaf00" :foreground "#272727")
                       (powerline-inactive1 :background "#b2b2b2" :foreground "#151515")
                       (powerline-active2 :background "#005fff" :foreground "#eeeeee")
                       (powerline-inactive2 :background "#8a8a8a" :foreground "#efefef")
                       (mode-line :foreground "#00875F"))))
  )


(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  (setq srecode-map-save-file "~/.emacs.d/.cache/srecode-map.el")
  ;; line numbers
  (setq-default dotspacemacs-line-number t
                dotspacemacs-auto-resume-layouts t)
  ;; evil settings
  (setq-default evil-escape-key-sequence "jk"
                evil-escape-delay 0.2)
  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-visual-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-visual-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-normal-state-map (kbd ";") 'evil-ex)
  ;; (define-key evil-normal-state-map (kbd "S-SPC") 'company-complete)
  ;;; evil-vimish-fold
  ;; (evil-vimish-fold-mode 1)
  (global-set-key (kbd "S-SPC") 'company-complete)
  ;; powerline
  (setq powerline-default-separator 'alternate)
  ;; Backups
  (setq backup-directory-alist `(("." . "~/tmp"))
        ;; uncomment next line for disabling backup files
        ;; make-backup-files nil
        )
  ;; disable automatic popups from auto-complete & company
  (setq ac-auto-show-menu nil
        company-idle-delay 0.5 ;; nil to disable it completely
        )
  ;;; Editing defaults
  (prefer-coding-system 'utf-8)
  (setq buffer-file-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (add-hook 'text-mode-hook 'auto-fill-mode)
  ;;; indentation - which one of these?
  ;; http://stackoverflow.com/questions/18172728/the-difference-between-setq-and-setq-default-in-emacs-lisp
  (setq-default indent-tabs-mode nil
                tab-width 4)
  ;;; Electric pairs
  (electric-indent-mode 1)
  (electric-pair-mode 1)
  ;; neotree
  (setq neo-theme 'ascii
        neo-show-hidden-files t)
  ;; Projectile
  ;; projectile-globally-ignored-files
  ;; projectile-globally-ignored-directories ".git" ".hg" ".idea" ".svn" "node_modules"
  ;; projectile-globally-ignored-file-suffixes
  ;; projectile-globally-ignored-modes
  (with-eval-after-load 'projectile
    (setq projectile-globally-ignored-directories
          (append projectile-globally-ignored-directories '(".git"
                                                            ".hg"
                                                            ".idea"
                                                            ".svn"
                                                            "node_modules"))
          projectile-globally-ignored-files
          (append projectile-globally-ignored-files '(".DS_Store")))
    (setq-default projectile-tags-file-name "tags"))
  ;; Markdown
  (with-eval-after-load 'markdown-mode
    (setq markdown-command "~/bin/emarkdown"
          markdown-open-command "~/bin/marked"
          markdown-italic-underscore t))

  ;; avy
  (setq-default avy-all-windows 'all-frames)

  ;; frame size
  (setq default-frame-alist '((width . 105)
                              (height . 65)))
)

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
