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
     ;; Generic
     (spell-checking :variables spell-checking-enable-by-default nil)
     ;; UI
     eyebrowse
     (ranger :variables ranger-show-preview t)
     themes-megapack
     theming
     ;; docs
     dash
     search-engine
     ;; git
     git
     github
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
   dotspacemacs-additional-packages
   '(
     evil-vimish-fold
     focus
     key-chord
     )
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
                         alect-dark ;; alect-dark-alt
                         flatui
                         misterioso
                         material-light
                         material
                         spacemacs-light
                         spacemacs-dark
                         leuven
                         gruvbox
                         anti-zenburn
                         zenburn
                         ample
                         heroku
                         monokai
                         solarized-light
                         solarized-dark)
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Operator Mono" ;; "PragmataPro Mono"h
                               :size  12
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
                       (powerline-active2 :background "#5fafd7" :foreground "#eeeeee") ;; afffff 00[5d]fff afd700
                       (powerline-inactive2 :background "#8a8a8a" :foreground "#efefef")
                       (mode-line :foreground "#00875F"))))
  )


;; http://stackoverflow.com/questions/18172728/the-difference-between-setq-and-setq-default-in-emacs-lisp
(defun dotspacemacs/user-config ()
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration."
  (setq srecode-map-save-file "~/.emacs.d/.cache/srecode-map.el")
  ;; line numbers
  (setq-default dotspacemacs-line-number t
                dotspacemacs-auto-resume-layouts t)
  ;; Backups
  (setq backup-directory-alist `(("." . "~/tmp"))
        ;; uncomment next line for disabling backup files
        ;; make-backup-files nil
        )
  ;;; Editing defaults
  (setq buffer-file-coding-system 'utf-8
        file-name-coding-system 'utf-8
        locale-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-language-environment 'utf-8)
  (set-terminal-coding-system 'utf-8)
  ;;; text-mode hooks
  (add-hook 'text-mode-hook 'auto-fill-mode)
  (add-hook 'text-mode-hook 'turn-on-flyspell)
  ;;; indentation
  (setq-default indent-tabs-mode nil
                tab-width 4)
  ;;; Electric pairs
  (electric-indent-mode 1)
  (electric-pair-mode 1)
  ;; auto-completion key
  (global-set-key (kbd "S-SPC") 'company-complete)
  ;;
  ;; evil settings
  ;;
  (setq-default evil-escape-key-sequence "fd"
                evil-escape-delay 0.2)
  ;;; key-chord
  (setq key-chord-two-keys-delay 0.2)
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" 'evil-escape)
  ;;; bindings
  (define-key evil-normal-state-map "j" 'evil-next-visual-line)
  (define-key evil-visual-state-map "j" 'evil-next-visual-line)
  (define-key evil-normal-state-map "k" 'evil-previous-visual-line)
  (define-key evil-visual-state-map "k" 'evil-previous-visual-line)
  (define-key evil-normal-state-map ";" 'evil-ex)
  ;; avy
  (spacemacs/declare-prefix "s?" "avy-goto")
  (spacemacs/set-leader-keys "SPC" 'evil-avy-goto-char)
  (spacemacs/set-leader-keys "s /" 'evil-avy-goto-word-or-subword-1)
  (spacemacs/set-leader-keys "s ?C" 'evil-avy-goto-char)
  (spacemacs/set-leader-keys "s ?c" 'evil-avy-goto-char-2)
  (spacemacs/set-leader-keys "s ?W" 'evil-avy-goto-word-0)
  (spacemacs/set-leader-keys "s ?w" 'evil-avy-goto-word-1)
  (spacemacs/set-leader-keys "s ?S" 'evil-avy-goto-subword-0)
  (spacemacs/set-leader-keys "s ?s" 'evil-avy-goto-subword-1)
  ;; frames
  (spacemacs/declare-prefix "F" "frames")
  (spacemacs/set-leader-keys "F b" 'display-buffer-other-frame)
  (spacemacs/set-leader-keys "F c" 'delete-frame)
  (spacemacs/set-leader-keys "F C" 'delete-other-frames)
  (spacemacs/set-leader-keys "F f" 'find-file-other-frame)
  (spacemacs/set-leader-keys "F RET" 'make-frame)
  (spacemacs/set-leader-keys "F n" 'make-frame)
  (spacemacs/set-leader-keys "F o" 'other-frame)
  (defun alpo/new-frame-with-layout-for-project ()
    "Creates a new frame, offer to open a project, and create a new layout
for it."
    (interactive)
    (make-frame)
    (spacemacs//layouts-spacemacs/helm-perspectives-l)
    ;; (spacemacs/layout-switch-by-pos 2)
    ;; (ido-find-file)
    (helm-find-files nil)
    ;; (dired-other-frame (helm-current-directory))
    )
  (spacemacs/set-leader-keys "F p" 'alpo/new-frame-with-layout-for-project)
  ;;; evil-vimish-fold
  ;; (evil-vimish-fold-mode 1)

  ;; powerline
  (setq powerline-default-separator 'alternate)
  ;; disable automatic popups from auto-complete & company
  (setq ac-auto-show-menu nil
        company-idle-delay 0.5 ;; nil to disable it completely
        )
  ;; ispell
  (setq-default ispell-program-name "aspell")
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
  (defun alpo/markdown-edit-mode ()
    "Markdown writing optimized environment"
    (interactive)
    (unless (local-variable-p 'alpo/markdown-in-edit-mode (current-buffer))
      (make-local-variable 'alpo/markdown-in-edit-mode)
      (setq alpo/markdown-in-edit-mode 0)
      (make-local-variable 'alpo/markdown-prev-theme)
      (setq alpo/markdown-prev-theme spacemacs--cur-theme))
    (message "alpo/markdown-in-edit-mode(1): %d" alpo/markdown-in-edit-mode)
    (if (eq alpo/markdown-in-edit-mode 0)
        (progn
          (switch-to-buffer-other-frame (current-buffer))
          (setq color-theme-is-global nil)
          (load-theme 'leuven t)
          (spacemacs/toggle-line-numbers-off)
          (spacemacs/toggle-fringe-off)
          (set-window-margins (selected-window) 20 20)
          (set-face-attribute 'default (selected-frame) :height 140)
          (set-frame-size (selected-frame)
                          (+ 20 20 (window-body-width (selected-window)))
                          (window-total-height (selected-window)))
          ;; (setq line-spacing 1.1) breaks the way text is displayed
          ;; this was an attempt to fix the frame not showing all text
          ;; when scrolling
          ;; (redraw-frame (selected-frame))
          ;; (redraw-display)
          (setq alpo/markdown-in-edit-mode 1)
          (message "alpo/markdown-in-edit-mode(2): %d (enabled) %s" alpo/markdown-in-edit-mode alpo/markdown-prev-theme)
          )
      (progn
        ;; not really necessary as they apply to the frame that will be deleted
        ;; (setq line-spacing 1)
        ;; (set-window-margins (selected-window) 0 0)
        ;; (spacemacs/toggle-line-numbers-on)
        (delete-frame)
        ;; (helm-themes--load-theme markdown-prev-theme)
        (mapc 'disable-theme custom-enabled-themes)
        (load-theme alpo/markdown-prev-theme t)
        (setq alpo/markdown-in-edit-mode 0)
        (message "alpo/markdown-in-edit-mode(2): %d (disabled)" alpo/markdown-in-edit-mode)
        (kill-local-variable 'alpo/markdown-in-edit-mode)
        (kill-local-variable 'alpo/markdown-prev-theme))
      )
    )
  ;; markdown
  (with-eval-after-load 'markdown-mode
    (setq markdown-command "~/bin/emarkdown"
          markdown-open-command "~/bin/marked"
          markdown-italic-underscore t)
    (spacemacs/set-leader-keys-for-major-mode 'markdown-mode "e" 'alpo/markdown-edit-mode)
    (spacemacs/set-leader-keys-for-major-mode 'markdown-mode "F" 'focus-mode))

  ;; avy
  (setq-default avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?z ?x ?c ?v ?b ?n ?m)
                avy-style 'at
                avy-case-fold-search nil
                avy-all-windows nil ;; nil, t, 'all-frames
                avy-background t)
  ;; ranger
  (setq-default ranger-override-dired t
                ranger-show-dotfiles t
                ranger-cleanup-on-disable t)
  ;; frame size
  (setq default-frame-alist '((width . 105)
                              (height . 65)))
)
;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
