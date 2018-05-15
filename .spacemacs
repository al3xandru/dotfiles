;; -*- mode: emacs-lisp; tab-width: 2; -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; Generic
     (ivy :variables
          ivy-count-format "%d/%d "
          ivy-use-virtual-buffers t)
     (spell-checking :variables
                     spell-checking-enable-by-default nil
                     spell-checking-enable-auto-dictionary nil)
     ;; UI
     ;; themes-megapack
     theming

     ;; source countrol
     ;; git
     ;; github
     ;; (version-control :variables
     ;;                  version-control-diff-tool 'diff-hl
     ;;                  version-control-global-margin t)
     emacs-lisp
     markdown
     org
     ;; languages
     ;; asciidoc
     ;; c-c++
     ;; csharp
     ;; go
     ;; html
     ;; java
     ;; javascript
     ;; php
     ;; python
     ;; ruby
     ;; scala
     ;; shell-scripts
     ;; swift
     ;; yaml

     ;; vim
     evil-commentary
     evil-snipe

     ;; misc
     ;; deft
     selectric
     xkcd
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     focus
     keyfreq
     key-chord
     org-alert
     (taskpaper-mode :location "~/Dropbox/workspaces/mine/emacs/taskpaper-mode/")
     ;; (taskpaper-mode :location (recipe
     ;;                            :fetcher github
     ;;                            :repo "al3xandru/taskpaper-mode"
     ;;                            ))
     ;; themes
     ;; autothemer
     ;; color-theme-sanityinc-tomorrow
     alect-themes
     darktooth-theme
     nova-theme
     subatomic-theme
     ;; zenburn-theme
     )
   ;; A list of packages that will not be install and loaded.
   dotspacemacs-excluded-packages
   '(
     vi-tilde-fringe
     ;; srefactor
     ;; space-doc ;; #5837
     ;; ;; java
     ;; emacs-eclim
     ;; ;; javascript
     ;; coffee-mode
     ;; company-tern
     ;; tern
     ;; ;; php
     ;; drupal-mode
     ;; spacemacs-layouts
     ;; eyebrowse
     )))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
    ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((bookmarks . 10)
                                (recents . 10)
                                (projects . 5))
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(subatomic
                         spacemacs-dark)
                        ;; Org aware: leuven manoj tango wombat
                        ;; misterioso
                        ;; leuven
                        ;; gruvbox
                        ;; flatui
                        ;; leuven
                        ;; ample
                        ;; heroku
                        ;; monokai
                        ;; anti-zenburn
                        ;; solarized-light
                        ;; solarized-dark
   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state nil
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   ;; "Hack"  "Input Mono" "Liberation Mono" "Operator Mono" "PragmataPro Mono" "SF Mono Regular"
   dotspacemacs-default-font '("Anka/Coder Narrow"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.0)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "M-m M-m"
   ;; THESE variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state t
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.6
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 100
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'.
   dotspacemacs-inactive-transparency 75
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers t
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg`, `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "ack" "grep")
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (setq theming-modifications
        '((alect-light (powerline-active1 :background "#ffaf00" :foreground "#272727")
                       (powerline-inactive1 :background "#b2b2b2" :foreground "#151515")
                       (powerline-active2 :background "#5fafd7" :foreground "#eeeeee") ;; afffff 00[5d]fff afd700
                       (powerline-inactive2 :background "#8a8a8a" :foreground "#efefef")
                       (mode-line :foreground "#00875F")
                       (org-level-2 :weight normal :height 1.1)
                       (org-level-3 :weight normal :height 1)
                       (org-level-4 :weight normal :height 1)
                       (org-level-5 :weight normal :height 1)
                       (org-level-6 :weight normal :height 1)
                       (org-level-7 :weight normal :height 1)
                       (org-level-8 :weight normal :height 1))
          (alect-dark (org-level-1 :height 1.1)
                      (org-level-2 :weight normal :height 0.9)
                      (org-level-3 :weight normal :height 0.9)
                      (org-level-4 :weight normal :height 0.9)
                      (org-level-5 :weight normal :height 0.9)
                      (org-level-6 :weight normal :height 0.9)
                      (org-level-7 :weight normal :height 0.9)
                      (org-level-8 :weight normal :height 0.9))
          ;; (nova (cursor :background "#3C4C55" :foreground "#DF8C8C"))
          (spacemacs-dark (ivy-current-match :background "#696676")
                          (org-level-2 :weight normal :height 1.1)
                          (org-level-3 :height 1.1)
                          (org-agenda-done :height 1.0 :foreground "#52676f" )
                          (org-scheduled-today :foreground "#e0211d" :height 1.1))))
  (setq eyebrowse-keymap-prefix (kbd "C-c C-y")))


;; http://stackoverflow.com/questions/18172728/the-difference-between-setq-and-setq-default-in-emacs-lisp
(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  ;; #5556
  (setq srecode-map-save-file "~/.emacs.d/.cache/srecode-map.el")
  ;; #2974/#5817
  (evil-define-key 'insert company-quickhelp-mode-map (kbd "C-k") 'company-select-previous)
  ;; (setq debug-on-error t)
  ;; frame size
  (setq default-frame-alist '((width . 111)
                              (height . 75)))
  ;; transparency
  ;; (spacemacs/toggle-transparency)
  ;; resume layouts?
  (setq-default dotspacemacs-auto-resume-layouts t)

  ;; Backups
  (setq backup-directory-alist `(("." . "~/tmp"))
        ;; disable lockfiles .#
        create-lockfiles nil
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

  (setq-default scroll-margin 5)
  ;;; indentation
  (setq-default indent-tabs-mode nil
                tab-width 4
                evil-shift-width 4
                standard-indent 4)
  (setq dired-use-ls-dired nil)

  ;;; Electric pairs
  (electric-indent-mode 1)
  (electric-pair-mode 1)
  ;; auto-completion key
  (global-set-key (kbd "S-SPC") 'company-complete)

  ;;; text-mode hooks
  (add-hook 'text-mode-hook #'auto-fill-mode)
  (add-hook 'text-mode-hook #'turn-on-flyspell)

  ;;
  ;; evil settings
  ;;
  ;; (setq-default evil-escape-key-sequence "jk"
  ;;               evil-escape-delay 0.2)
  ;;; key-chord
  (setq key-chord-two-keys-delay 0.2)
  (key-chord-mode 1)
  ;; (key-chord-define evil-insert-state-map "fd" #'evil-escape)
  ;;; bindings
  (define-key evil-normal-state-map "j" #'evil-next-visual-line)
  (define-key evil-visual-state-map "j" #'evil-next-visual-line)
  (define-key evil-normal-state-map "k" #'evil-previous-visual-line)
  (define-key evil-visual-state-map "k" #'evil-previous-visual-line)
  (define-key evil-normal-state-map ";" #'evil-ex)
  (defun alpo/autopairs-shortcut-jump ()
    (interactive)
    (backward-up-list 1)
    (forward-sexp 1))
  (define-key evil-insert-state-map (kbd "M-9") #'alpo/autopairs-shortcut-jump)
  (define-key evil-insert-state-map (kbd "M-0") #'evil-lisp-state-sp-forward-slurp-sexp)

  ;; avy
  ;; in 0.200 jumps are exposed in SPC j 
  ;; (spacemacs/set-leader-keys "," 'evil-avy-goto-char)
  ;; (spacemacs/set-leader-keys "s SPC" 'evil-avy-goto-word-or-subword-1)
  ;; (spacemacs/declare-prefix "sx" "avy-goto")
  ;; (spacemacs/set-leader-keys "s xc" 'evil-avy-goto-char-2)
  ;; (spacemacs/set-leader-keys "s xC" 'evil-avy-goto-char)
  ;; (spacemacs/set-leader-keys "s xw" 'evil-avy-goto-word-1)
  ;; (spacemacs/set-leader-keys "s xW" 'evil-avy-goto-word-0)
  ;; (spacemacs/set-leader-keys "s xs" 'evil-avy-goto-subword-1)
  ;; (spacemacs/set-leader-keys "s xS" 'evil-avy-goto-subword-0)
  ;; frames
  (spacemacs/declare-prefix "F" "frames")
  (spacemacs/set-leader-keys "Fb" #'display-buffer-other-frame)
  (spacemacs/set-leader-keys "Fd" #'delete-frame)
  (spacemacs/set-leader-keys "FD" #'delete-other-frames)
  (spacemacs/set-leader-keys "Ff" #'find-file-other-frame)
  (spacemacs/set-leader-keys "F RET" #'make-frame)
  (spacemacs/set-leader-keys "Fn" #'make-frame)
  (spacemacs/set-leader-keys "Fo" #'other-frame)
  (defun alpo/new-frame-with-layout-for-project ()
    "Creates a new frame, offer to open a project, and create a new layout
for it."
    (interactive)
    (make-frame)
    (spacemacs//layouts-spacemacs/helm-perspectives-l)
    ;; (spacemacs/layout-switch-by-pos 2)
    ;; (ido-find-file)
    ;; (helm-find-files nil)
    (counsel-find-file nil)
    ;; (dired-other-frame (helm-current-directory))
    )
  (spacemacs/set-leader-keys "Fp" #'alpo/new-frame-with-layout-for-project)

  ;;; evil-vimish-fold
  ;; (evil-vimish-fold-mode 1)

  ;; auto-complete/company
  ;; disable automatic popups from auto-complete & company
  (setq ac-auto-show-menu nil
        company-idle-delay 0.5 ;; nil to disable it completely
        )

  ;; avy
  (setq-default avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?z ?x ?c ?v ?b ?n ?m)
                avy-style 'at
                avy-case-fold-search nil
                avy-all-windows nil ;; nil, t, 'all-frames
                avy-background t)
  ;; deft
  (setq deft-directory "~/Dropbox/Dox/nvall"
        deft-extensions '("md" "txt" "org"))
  ;; diff
  (setq diff-hl-side 'left)

  ;; helm
  ;; helm input at the bottom
  ;; (setq-default helm-echo-input-in-header-line nil)

  ;; help
  ;; learn emacs
  ;; http://sachachua.com/blog/2016/02/building-today-learned-habit-displaying-documentation-random-emacs-commands/
  (defun alpo/describe-random-interactive-function ()
    (interactive)
    "Show the documentation for a random interactive function.
Consider only documented, non-obsolete functions."
    (let (result)
      (mapatoms
       (lambda (s)
         (when (and (commandp s)
                    (documentation s t)
                    (null (get s 'byte-obsolete-info)))
           (setq result (cons s result)))))
      (describe-function (elt result (random (length result))))))
  (spacemacs/set-leader-keys "hr" #'alpo/describe-random-interactive-function)

  ;; ispell
  (setq-default ispell-program-name "aspell")

  ;; ivy
  (setq ivy-wrap t
        ivy-extra-directories '())

  ;; keyfreq
  ;; http://blog.binchen.org/posts/how-to-be-extremely-efficient-in-emacs.html
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-excluded-commands
        '(self-insert-command))

  ;; Markdown
  ;; (defun alpo/markdown-hook ()
  ;;   (define-key evil-insert-state-map (kbd "C-c C-d") 'ispell-complete-word))
  ;; (add-hook 'markdown-mode-hook 'alpo/markdown-hook)
  (defun alpo/ispell-complete-word ()
    (interactive)
    (save-excursion
      (ispell-complete-word))
    (forward-word))
  (evil-define-key 'insert markdown-mode-map (kbd "C-c C-d") #'alpo/ispell-complete-word)

  (setq-default markdown-edit-mode-theme 'leuven
                markdown-edit-mode-margin 20
                markdown-edit-mode-face-height 140)
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
          (load-theme markdown-edit-mode-theme t)
          (spacemacs/toggle-line-numbers-off)
          (spacemacs/toggle-fringe-off)
          (set-window-margins (selected-window) markdown-edit-mode-margin markdown-edit-mode-margin)
          (set-face-attribute 'default (selected-frame) :height markdown-edit-mode-face-height)
          (set-frame-size (selected-frame)
                          (+ 20 (* 2 markdown-edit-mode-margin) (window-body-width (selected-window)))
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
        (spacemacs/toggle-line-numbers-on)
        (setq linum-format "%4d")
        ;; (helm-themes--load-theme markdown-prev-theme)
        (mapc 'disable-theme custom-enabled-themes)
        (load-theme alpo/markdown-prev-theme t)
        (setq alpo/markdown-in-edit-mode 0)
        (message "alpo/markdown-in-edit-mode(2): %d (disabled)" alpo/markdown-in-edit-mode)
        (kill-local-variable 'alpo/markdown-in-edit-mode)
        (kill-local-variable 'alpo/markdown-prev-theme))
      ))

  (setq-default markdown-asymmetric-header t
                markdown-command "~/bin/emarkdown"
                markdown-open-command "/Applications/Marked 2.app/Contents/MacOS/Marked 2"
                markdown-italic-underscore t
                markdown-css-paths '("https://maxcdn.bootstrapcdn.com/bootstrap/3.3.6/css/bootstrap.min.css"))
  (spacemacs/set-leader-keys-for-major-mode 'markdown-mode "e" #'alpo/markdown-edit-mode)
  (spacemacs/set-leader-keys-for-major-mode 'markdown-mode "F" #'focus-mode)
  ;; (spacemacs/set-leader-keys-for-major-mode 'markdown-mode "SPC" 'ispell-complete-word)

  ;; neotree
  (setq neo-theme 'ascii
        neo-show-hidden-files t)

  ;; powerline
  (setq powerline-default-separator 'alternate)

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

  ;; Python
  ;;; Anaconda
  (add-hook 'python-mode-hook
            (lambda ()
              (setq python-shell-interpreter "python")
              ;; (setq anaconda-mode-server-script "../anaconda_mode.py")
              ))
  (setq-default python-shell-interpreter "python")

  ;; ranger
  (setq-default ranger-override-dired t
                ranger-show-dotfiles t
                ranger-cleanup-on-disable t)

  ;; taskpaper
  (require 'taskpaper-mode)
  (setq taskpaper-append-date-to-done t)
  (with-eval-after-load 'taskpaper-mode
    ;; (yas-minor-mode-on)
    (font-lock-add-keywords 'taskpaper-mode
                            '(
                              ("@i\(must\)" . font-lock-string-face)
                              ("@i(want)" . font-lock-type-face)
                              ("@goal" . font-lock-string-face)
                              ("@important" . font-lock-keyword-face))))

  ;; twitter
  (setq-default twittering-icon-mode nil
                twittering-reverse-mode t)
  ;;
  ;; ORG
  ;;
  (defun alpo/switch-evil-to-emacs ()
    (if (evil-emacs-state-p)
        (message "Expected Vim mode, but found Emacs mode.  Weird!")
      (progn
        (evil-emacs-state)
        ;; (setq unread-command-events (listify-key-sequence "\C-z"))
        (message "Switching to Emacs mode"))))


  (add-hook 'org-capture-mode-hook #'alpo/switch-evil-to-emacs)
  (add-hook 'org-mode-hook #'alpo/switch-evil-to-emacs)
  (add-hook 'orgstruct-mode-hook #'alpo/switch-evil-to-emacs)
  (add-hook 'org-load-hook #'alpo/switch-evil-to-emacs)
  (add-hook 'orgstruct-setup-hook #'alpo/switch-evil-to-emacs)

  (with-eval-after-load 'org
    (add-to-list 'org-modules 'org-habit)
    (setq org-bullets-bullet-list '("✸" "◉" "○" "◆" "▶")
          org-hide-leading-stars t
          org-startup-indented t
          org-use-speed-commands t)

    (setq org-directory "~/Dropbox/Dox/mydox/"
          org-default-notes-file (concat org-directory "mlo.org")
          org-agenda-files (mapcar (lambda (f) (concat org-directory f)) (list "mlo.org"
                                                                               "11-inbox.org"
                                                                               "12-habits.org")))

    ;; check https://github.com/pisemsky/dotfiles/blob/b287adf0278ffc55e4fa47ee083c36e72c6ab751/.emacs.d/lisp/init-org.el
    (setq org-refile-targets '((org-agenda-files :maxlevel .  4)
                               ("99-org-cheatsheet.org" :maxlevel .  8))
          org-refile-use-outline-path 'file
          org-outline-path-complete-in-steps nil
          org-refile-allow-creating-parent-nodes t)

    (setq org-archive-location "orgdata/archive/%s_archive::"
          org-attach-directory "orgdata/attachments/")

    ;; MobileOrg https://orgmode.org/org.html#MobileOrg
    (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg"
          org-mobile-inbox-for-pull "~/Dropbox/Dox/mydox/11-inbox.org")

    (setq org-log-done t
          org-log-into-drawer "LOGBOOK")
    (setq org-enforce-todo-dependencies t
          org-track-ordered-property-with-tag t
          org-agenda-dim-blocked-tasks t)
    ;; enable on a per-file basis #+STARTUP: customtime
    ;; (setq org-time-stamp-custom-formats '("<%a,%b.%d>" . "<%a,%b.%d %H:%M>")
    ;;       org-display-custom-times t)
    ;; Past sequences
    ;; '((sequence "TODO(t)" "WANT(w)" "MUST(m)" "|" "DONE(d)")
    ;;   (sequence "WIPR(p)" "WAIT(s@/!)" "|" "FILED(f)" "SKIP(x@/!)")))
    ;; '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
    ;;   (sequence "WIPR(p)" "WAIT(w@/!)" "|" "DONE(d)" "FILED(f)" "SKIP(x@/!)")))
    (setq org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)" )
                              (sequence "WAIT(w@/!)" "SOMEDAY(s)" "HOLD(h@/!)" "|" "SKIP(x@/!)")
                              (sequence "PRJ" "|")))

    (setq org-todo-keyword-faces '(("TODO" . (:foreground "#dc752f" :weight normal))
                                   ("NEXT" . (:foreground "#c61b6e" :weight bold))
                                   ("WAIT" . (:foreground "#5859b7" :slant italic))
                                   ("HOLD" . (:foreground "#2075c7" :slant italic))
                                   ("DONE" . (:foreground "#52676f" :weight normal :strike-through t))
                                   ("SKIP" . (:foreground "#465a61" :weight normal :slant italic :strike-through t))
                                   ("SOMEDAY" .  (:foreground "#a57705" :weight normal))
                                   ("PRJ" .  (:foreground "#4f97d7" :weight bold :height 1.2))))
    ;; Past keyword faces
    ;; ("TODO" . (:foreground "#cd4f39"))
    ;; ("NEXT" . (:foreground "#6c71c4" :weight bold))
    ;; ("DONE" . (:foreground "#698b69"))
    ;; ("WAIT" . (:foreground "#b58900" :slant italic))
    ;; ("HOLD"  . (:foreground "#698b69" :slant italic))
    ;; ("SKIP" . (:foreground "#ffd39b" :weight normal :strike-through t))

    ;; ("FILED" . (:foreground "#698b69" :slant italic))
    ;; ("WIPR" . (:foreground "#268bd2" :weight bold))
    ;; ("MUST" . (:foreground "#fe2500" :weight bold))
    ;; ("WANT" . (:foreground "#cc1b00" :weight bold :slant italic))
    ;; ("WISH" . (:foreground "#cd4f39" :slant italic))))

    (setq org-capture-templates
          '(("t"
             "* TODO"
             entry
             (file+datetree org-default-notes-file)
             "* TODO %?\n:PROPERTIES:\n:CREATED: %u\n:END:\n%i")
            ("e"
             "* TODO Respond to email"
             entry
             (file+datetree org-default-notes-file)
             "* TODO Respond to email subject:(%^{mail|zol[mp]}) from:(%^{name|none}) :email:\n:PROPERTIES:\n:CREATED: %u\n:END:")
            ("u"
             "* TODO with link in clipboard"
             entry
             (file+datetree org-default-notes-file)
             "* TODO %^{Action|Read|Check|Bookmark} \"[[%c][%?]]\"\n:PROPERTIES:\n:CREATED: %u\n:END:")
            ("U"
             "* TODO from link"
             entry
             (file+datetree org-default-notes-file)
             "* TODO %^{Action|Read|Check|Bookmark} \"%?\"\n:PROPERTIES:\n:CREATED: %u\n:END:")
            ("r"
             "* SOMEDAY Read later link in clipboard"
             entry
             (file+olp org-default-notes-file "Read later")
             "* SOMEDAY %^{Action|Read|Check|Bookmark} \"[[%c][%?]]\"\n:PROPERTIES:\n:CREATED: %u\n:END:")
             ("R"
              "* SOMEDAY Read later from link"
              entry
              (file+olp org-default-notes-file "Read later")
              "* SOMEDAY %^{Action|Read|Check|Bookmark} \"%?\"\n:PROPERTIES:\n:CREATED: %u\n:END:")
             ("m"
              "* Meeting"
              entry
              (file+datetree org-default-notes-file)
              "* MEETING %? :meeting:\n:MEETING:\n:CREATED: %U:\n:PEOPLE:\n:RECORDING:\n:END:"
              :clock-in t
              :clock-keep t)

             ("c"
              "- [ ] Checkbox"
              checkitem
              (file+datetree org-default-notes-file)
              "- [ ] %?\n%U\n  %i")
             ("h"
              "* Heading [H:M]"
              entry
              (file+datetree org-default-notes-file)
              "* %? [%<%H:%M>]")
             ("+"
              "Log (+ [H:M])"
              item
              (file+datetree org-default-notes-file)
              "+ [%<%H:%M>] %?")
             ("n"
              "Note"
              item
              (file+datetree org-default-notes-file)
              "+ %? %U")
             ("p"
              "Project"
              entry
              (file+olp org-default-notes-file "Projects")
              "* PRJ %? [%]\n** NEXT Write down what is the purpose or what are the goals/objectives for this project\n** TODO Write down what are the deliverables or what does it mean to be done\n** TODO Brainstorm initial set of tasks\n** TODO Organize tasks\n** TODO Identify next action")

             ("x" "Using clipboard")
             ("xt" "* TODO with clipboard" entry (file+datetree org-default-notes-file)
              "* TODO %?\n%c\n:PROPERTIES:\n:CREATED: %u\n:END:\nCREATED: %U\n%i")
             ("xc" "- [ ] Checbox with clipboard" checkitem (file+datetree org-default-notes-file) "- [ ] %?\n%c\n%U\n%i")
             ("xn" "Note with clipboard" item (file+datetree org-default-notes-file) "+ %? %U\n%c")

             ("w" "Using references")
             ("wt" "* TODO with ref" entry (file+datetree org-default-notes-file)
              "* TODO %?\n:PROPERTIES:\n:CREATED: %u\n:END:\nCREATED: %U\n%a\n%i")
             ("wc" "- [ ] Checkbox with ref" checkitem (file+datetree org-default-notes-file)
              "- [ ] %?\n%U\n%a\n%i")

             ("L" "Library")
             ("Lp" "Pick up book" entry (file+olp org-default-notes-file "Tasks") "* TODO Pick up book from library \"%^{Book}\" :#me:@library:\nDEADLINE: %^T")
             ("Lr" "Return book to library" entry (file+olp org-default-notes-file "Tasks") "* TODO Return book to library \"%^{Book}\" :#me:@library:\nDEADLINE: %^T")

             ("*"
              "Random todo"
              entry
              (file+datetree org-default-notes-file)
              "* %?\n:PROPERTIES:\n:CREATED: %u\n:END:\n%i")
          ))2

    (setq org-agenda-span 14
          org-agenda-start-on-weekday nil) ;; nil: begin on current day, t: begin Monday

    (setq org-agenda-custom-commands
          '(("w" "Weekly tasks"
             (
              (agenda "Agenda"
                      ((org-agenda-entry-types '(:deadline :scheduled :timestamp))
                       (org-agenda-span 'week)
                       (org-deadline-warning-days 0)
                       (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))))

              (todo "NEXT"
                    ((org-agenda-overriding-header "Next actions:")
                     (org-agenda-sorting-strategy '(todo-state-up priority-down))))

              ;; (tags-todo "DEADLINE<\"<+5d>\"|SCHEDULED<\"<+5d>\""
              ;;            ((org-agenda-overriding-header "Scheduled for next 5 days:")
              ;;             (org-agenda-sorting-strategy '(deadline-up scheduled-up todo-state-up priority-down habit-down))
              ;;             (org-agenda-files (mapcar (lambda (f) (concat org-directory f)) (list "mlo.org"
                                                                                                 ;; "11-inbox.org")))))
        
              (todo "WAIT|HOLD"
                    ((org-agenda-overriding-header "Parked/Waiting")
                     (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))))

              (tags-todo "notes/!TODO|NEXT"
                         ((org-agenda-overriding-header "Refile?")
                          (org-tags-match-list-sublevels t)))
              )

             ;; Uncomment next 2 lines to change the format of the view
             ;; ((org-columns-default-format "%CATEGORY %5TODO %1PRIORITY %20SCHEDULED %20DEADLINE %ITEM")
             ;;  (org-agenda-view-columns-initially t))
             )
            ("A" "Task ready to archive"
             (
              (tags-todo "CLOSED<\"<-30d>\"+TODO=\"DONE\"|TODO=\"SKIP\""
                         ((org-agenda-overriding-header "Archive")
                          (org-tags-match-list-sublevels nil)))
              ))
            ("B" "Backlog"
              ((tags-todo "DEADLINE<\"<+1d>\"/!TODO|NEXT"
                    ((org-agenda-overriding-header "Urgent deadlines (today and past):")
                     (org-agenda-sorting-strategy '(habit-down deadline-down priority-down))))

              (tags-todo "TIMESTAMP<\"<+1d>\"|SCHEDULED<\"<+1d>\"/!TODO|NEXT"
                     ((org-agenda-overriding-header "Available now:")
                      (org-agenda-sorting-strategy '(habit-down ts-down scheduled-down priority-down))
                      (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottimestamp 'deadline))))

              (tags-todo "+DEADLINE>\"<now>\"+DEADLINE<=\"<+1w>\"/!TODO|NEXT"
                         ((org-agenda-overriding-header "Deadlines in next 7 days:")
                          (org-agenda-sorting-strategy '(habit-down deadline-up priority-down))))

              (tags-todo "+TIMESTAMP>\"<now>\"+TIMESTAMP<\"<+8d>\"|+SCHEDULED>\"<now>\"+SCHEDULED<\"<+8d>\"/!TODO|NEXT"
                         ((org-agenda-overriding-header "Planned for next week:")
                          (org-agenda-sorting-strategy '(habit-down timestamp-up priority-down))
                          (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))))


              (tags-todo "-DEADLINE<=\"<+1w>\"&-SCHEDULED<=\"<+1w>\"/!TODO|NEXT"
                         ((org-agenda-overriding-header "Backlog")
                          (org-agenda-sorting-strategy '(timestamp-up priority-down))))


              ))
            ))


    (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
                                                             (shell .  t)
                                                             (sh .  t)
                                                             (dot .  t)
                                                             (org .  t)))
    (setq org-export-coding-system 'utf-8)

    ;; org-habit
    (setq org-habit-preceding-days 14
          org-habit-graph-column 60)

    ;; org-alert
    ;; (require 'org-alert)
    ;; (setq alert-default-style 'osx-notifier)
    ;; (org-alert-enable)

    ;; custom links
    ;; http://orgmode.org/manual/Adding-hyperlink-types.html#Adding-hyperlink-types
    ;; org-add-link-type is deprecated and replaced with http://kitchingroup.cheme.cmu.edu/blog/2016/11/04/New-link-features-in-org-9/
    ;; (org-add-link-type "dayone" #'alpo/org-dayone-open-link)
    ;; (org-add-link-type "dayone2" #'alpo/org-dayone-open-link)
    (org-link-set-parameters
     "dayone"
     :follow (lambda (path) (alpo/org-open-typed-link "dayone2" path))
     :face '(:foreground "#42b2fc" :underline t)
     :help-echo "Open in DayOne")
    (org-link-set-parameters
     "dayone2"
     :follow (lambda (path) (alpo/org-open-typed-link "dayone2" path))
     :face '(:foreground "#42b2fc" :underline t)
     :help-echo "Open in DayOne")
    ;; (org-add-link-type "mailplane" #'alpo/org-mailplane-open-link)
    (org-link-set-parameters
     "mailplane"
     :follow (lambda (path) (alpo/org-open-typed-link "mailplane" path))
     :face '(:foreground "#2f71e1" :underline t)
     :help-echo "Open in Mailplane")
    ;; (org-add-link-type "message" #'alpo/org-applemail-open-link)
    (org-link-set-parameters
     "message"
     :follow (lambda (path) (alpo/org-open-typed-link "message" path))
     :face '(:foreground "#1862de" :underline t)
     :help-echo "Open Mail")
    ;; (org-add-link-type "omnifocus" #'alpo/org-omnifocus-open-link)
    (org-link-set-parameters
     "omnifocus"
     :follow (lambda (path) (alpo/org-open-typed-link "omnifocus" path))
     :face '(:foreground "#a249ff" :underline t)
     :help-echo "Open in OmniFocus")
    ;; (org-add-link-type "twodo" #'alpo/org-2do-open-link)
  )

  (defun alpo/org-open-typed-link (type path)
    "Open an URL using a custom scheme"
    (let ((escpath (replace-regexp-in-string " " "%20" path)))
      (message "Open link: %s:%s" type escpath)
      (shell-command (format "open \"%s:%s\"" type escpath))))


  ;; Copy URL from Org
  ;; https://emacs.stackexchange.com/questions/3981/how-to-copy-links-out-of-org-mode
  (defun alpo/org-retrieve-url-at-point ()
    (interactive)
    (let* ((link-info (assoc :link (org-context)))
           (text (when link-info
                   (buffer-substring-no-properties (or (cadr link-info) (point-min))
                                                   (or (caddr link-info) (point-max))))))
      (if (not text)
          (error "Not in Org link")
        (add-text-properties 0 (length text) '(yank-handler (alpo/org-yank-link)) text)
        (kill-new text))))

  (defun alpo/org-yank-link (text)
    (if (derived-mode-p 'org-mode)
        (insert text)
      (string-match org-bracket-link-regexp text)
      (insert (substring text (match-beginning 1) (match-end 1)))))

  (global-set-key (kbd "C-c M-c") 'alpo/org-retrieve-url-at-point)
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "y" 'alpo/org-retrieve-url-at-point)


  ;; Notifications using appt
  ;; Based on http://doc.norang.ca/org-mode.html#Reminders
  ;; and https://gist.github.com/squiter/4475c96365575b7237c4
  (setq appt-audible t
        appt-disp-window-function (function alpo/osx-appt-display)
        appt-delete-window-function (function appt-delete-window)
        appt-display-format 'window
        appt-display-duration 90
        appt-display-interval 5
        appt-message-warning-time 15)

  (defun alpo/org-agenda-to-appt ()
    (interactive)
    (setq appt-time-msg-list nil)
    (org-agenda-to-appt))

  (defsubst alert-encode-string (str)
    (encode-coding-string str (keyboard-coding-system)))

  (defun alpo/osx-appt-display (min-to-appt new-time msg)
    (or (listp min-to-appt)
        (setq min-to-appt (list min-to-appt)
              msg (list msg)
              new-time (list new-time)))
    (dotimes (i (length msg))
      (if (string= "0" (nth i min-to-appt))
          (apply #'call-process "osascript" nil nil nil "-e"
                 (list (format "display notification %S with title %S subtitle %S"
                               (alert-encode-string (nth i msg))
                               (alert-encode-string "Org")
                               (alert-encode-string "Now"))))
 
        (apply #'call-process "osascript" nil nil nil "-e"
               (list (format "display notification %S with title %S subtitle %S"
                             (alert-encode-string (nth i msg))
                             (alert-encode-string "Org")
                             (alert-encode-string (format "In %s minutes" (nth i min-to-appt)))))))
      (message "appt: %s in %s minutes at %s" (nth i msg) (nth i min-to-appt) (nth i new-time))))


  (defun appt-delete-window()
    "Nothing. overwrite built-in func to avoid error: Error running timer ‘appt-delete-window’: (error \"No buffer named *appt-buf*\")")

  ;; Activate appointments so we get notifications
  ;; (appt-activate t)
  ;; This is at the end of my .emacs - so appointments are set up when Emacs starts
  (alpo/org-agenda-to-appt)
  ;; If we leave Emacs running overnight - reset the appointments one minute after midnight
  (run-at-time "24:01" 3600 'alpo/org-agenda-to-appt)
  ;; Rebuild the reminders everytime the agenda is displayed
  (add-hook 'org-finalize-agenda-hook 'alpo/org-agenda-to-appt 'append)

;; Load local customizations (local to the computer)
;; (when (file-exists-p "~/local.el")
;;   (load "~/local.el")))
)
;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#32302F" "#FB4934" "#B8BB26" "#FABD2F" "#83A598" "#D3869B" "#17CCD5" "#EBDBB2"])
 '(beacon-color "#f2777a")
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(custom-safe-themes
   (quote
    ("2941526f0165b681482a7bfe61b071db10c6df090d04a530c593254ea6412054" default)))
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-color "#37474f" t)
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(hl-sexp-background-color "#1c1f26")
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (alect-themes subatomic-theme subatomic256-theme darktooth-theme zenburn-theme color-theme-sanityinc-tomorrow flyspell-correct counsel smartparens helm-core async projectile hydra nova-theme org-alert gruvbox-light-theme material-theme autothemer evil helm org-plus-contrib magit ghub ivy org-projectile org-category-capture org-present org-pomodoro alert log4e gntp org-mime org-download htmlize gnuplot xkcd ws-butler winum which-key wgrep volatile-highlights uuidgen use-package toc-org spaceline smex smeargle selectric-mode restart-emacs request rainbow-delimiters popwin persp-mode pcre2el paradox orgit org-bullets open-junk-file neotree move-text monokai-theme mmm-mode markdown-toc magit-gitflow magit-gh-pulls macrostep lorem-ipsum linum-relative link-hint keyfreq key-chord ivy-hydra indent-guide hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation heroku-theme helm-make gruvbox-theme google-translate golden-ratio gitignore-mode github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md focus flyspell-correct-ivy flx-ido flatui-theme fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-snipe evil-search-highlight-persist evil-numbers evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-commentary evil-args evil-anzu eval-sexp-fu elisp-slime-nav dumb-jump diminish diff-hl define-word counsel-projectile column-enforce-mode clean-aindent-mode auto-highlight-symbol auto-dictionary auto-compile anti-zenburn-theme ample-theme aggressive-indent adaptive-wrap ace-window ace-link)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#36473A")
 '(pos-tip-foreground-color "#FFFFC8")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Anka/Coder Narrow" :foundry "nil" :slant normal :weight normal :height 130 :width extra-condensed))))
 '(ivy-current-match ((t (:background "#696676"))))
 '(org-agenda-done ((t (:height 1.0 :foreground "#52676f"))))
 '(org-level-1 ((t (:height 1.1))))
 '(org-level-2 ((t (:weight normal))))
 '(org-level-3 ((t (:weight normal))))
 '(org-level-4 ((t (:weight normal))))
 '(org-level-5 ((t (:weight normal))))
 '(org-level-6 ((t (:weight normal))))
 '(org-level-7 ((t (:weight normal))))
 '(org-level-8 ((t (:weight normal))))
 '(org-scheduled-today ((t (:foreground "#e0211d" :height 1.1)))))
