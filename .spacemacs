;; -*- mode: emacs-lisp; tab-width: 2; -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     ;; Generic
     ivy
     (spell-checking :variables
                     spell-checking-enable-by-default nil
                     spell-checking-enable-auto-dictionary t)
     ;; UI
     theming
     ;; source countrol
     git
     github
     (version-control :variables
                      version-control-diff-tool 'diff-hl
                      version-control-global-margin t)
     ;; languages
     emacs-lisp
     markdown
     org
     ;; languages
     asciidoc
     c-c++
     csharp
     go
     html
     java
     javascript
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
     unimpaired
     ;; misc
     deft
     xkcd
     ;;; previous setup
     ;; dash
     ;; ;; cscope
     ;; semantic
     ;; (syntax-checking :variables
     ;;                  syntax-checking-enable-by-default nil
     ;;                  syntax-checking-enable-tooltips nil)
     ;; ;; completion
     ;; (auto-completion :variables
     ;;                  auto-completion-enable-help-tooltip t
     ;;                  auto-completion-enable-snippets-in-popup t)
     ;;; misc unused
     ;; imenu-list
     ;; (ranger :variables ranger-show-preview t)
     ;; ;; twitter
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     ;; evil-vimish-fold
     focus
     keyfreq
     key-chord
     (taskpaper-mode :location "~/Dropbox/workspace/emacs/taskpaper-mode/")
     ;; (taskpaper-mode :location (recipe
     ;;                            :fetcher github
     ;;                            :repo "al3xandru/taskpaper-mode"
     ;;                            ))
     )
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages
   '(
     vi-tilde-fringe
     ;; srefactor
     ;; space-doc ;; #5837
     ;; ;; java
     emacs-eclim
     ;; ;; javascript
     coffee-mode
     company-tern
     tern
     ;; ;; php
     drupal-mode
     )
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
   dotspacemacs-startup-lists '((bookmarks . 10)
                                (recents . 10)
                                (projects . 5))
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
   ;; dotspacemacs-startup-recent-list-size 10
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(alect-dark
                         alect-light ;; alect-dark-alt
                         spacemacs-dark
                         spacemacs-light
                         misterioso
                         flatui
                         gruvbox
                         leuven
                         material
                         material-light
                         ;; zenburn
                         ample
                         heroku
                         monokai
                         anti-zenburn)
                         ;; solarized-light
                         ;; solarized-dark
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   ;; "Hack"  "Input Mono" "Liberation Mono" "Operator Mono" "PragmataPro Mono" "SF Mono Regular"
   dotspacemacs-default-font '("PragmataPro Mono"
                               :size  12
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab t
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
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
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
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "grep" "ack")
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
                       (mode-line :foreground "#00875F"))))
  )


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
  (setq default-frame-alist '((width . 105)
                              (height . 65)))
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
  (setq-default evil-escape-key-sequence "jk"
                evil-escape-delay 0.2)
  ;;; key-chord
  (setq key-chord-two-keys-delay 0.2)
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "fd" #'evil-escape)
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
  (with-eval-after-load 'org
    (setq org-directory "~/Dropbox/Dox/active/"
          org-default-notes-file "04-notes.org"
          org-agenda-files (list org-default-notes-file))

    ;; (setq org-log-done t
    ;;       org-todo-keywords '((sequence "TODO(t)" "WANT(w)" "MUST(m)" "|" "DONE(d)")
    ;;                           (sequence "WIPR(p)" "WAIT(s@/!)" "|" "FILED(f)" "SKIP(x@/!)")))
    (setq org-log-done t
          org-todo-keywords '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                              (sequence "WIPR(p)" "WAIT(w@/!)" "|" "DONE(d)" "FILED(f)" "SKIP(x@/!)")))
    ;; colors: 6495ed
    (setq org-todo-keyword-faces '(("TODO" . (:foreground "#cd4f39"))
                                   ("NEXT" . (:foreground "#6c71c4" :weight bold))
                                   ("DONE" . (:foreground "#698b69"))
                                   ("FILED" . (:foreground "#698b69" :slant italic))
                                   ("WIPR" . (:foreground "#268bd2" :weight bold))
                                   ("WAIT" . (:foreground "#b58900" :slant italic))
                                   ("SKIP" . (:foreground "#ffd39b" :weight normal :strike-through t))
                                   ("MUST" . (:foreground "#fe2500" :weight bold))
                                   ("WANT" . (:foreground "#cc1b00" :weight bold :slant italic))
                                   ("WISH" . (:foreground "#cd4f39" :slant italic))))

     (setq org-capture-templates
        '(("l" "Log (+ [H:M])" item (file+datetree org-default-notes-file)
           "+ [%<%H:%M>] %?")
          ("h" "Heading [H:M]" entry (file+datetree org-default-notes-file)
           "* %? [%<%H:%M>]")
          ("t" "Todo" entry (file+datetree org-default-notes-file)
           "* TODO %?\n  %U\n  %i")
          ("T" "Todo with ref" entry (file+datetree org-default-notes-file)
           "* TODO %?\n  %U\n  %a\n  %i")
          ("v" "Todo with clipboard" entry (file+datetree org-default-notes-file)
           "* TODO %?\n  %c\n  %U\n  %i")
          ("c" "Task (- [])" checkitem (file+datetree org-default-notes-file)
           "- [ ] %?\n  %U\n  %i")
          ("C" "Task with ref" checkitem (file+datetree org-default-notes-file)
           "- [ ] %?\n  %U\n  %a\n  %i")
          ("V" "Task with clipboard" checkitem (file+datetree org-default-notes-file)
           "- [ ] %?\n  %c\n  %U\n  %i")
          ("n" "Note" item (file+datetree org-default-notes-file)
           "+ %? %U")
          ("N" "Note with clipboard" item (file+datetree org-default-notes-file)
           "+ %? %U\n  %c")
          ("m" "Meeting" entry (file+datetree org-default-notes-file)
           "* MEETING with %? %U" :clock-in t)))

    (setq org-agenda-span 14
          org-agenda-start-on-weekday 1)

    ;; http://orgmode.org/manual/Adding-hyperlink-types.html#Adding-hyperlink-types
    (org-add-link-type "twodo" #'alpo/org-2do-open-link)
    (org-add-link-type "mailplane" #'alpo/org-mailplane-open-link)
    (org-add-link-type "omnifocus" #'alpo/org-omnifocus-open-link))

(defun alpo/org-omnifocus-open-link (path)
  "Open an omnifocus url scheme"
  (message "Open OmniFocus link: omnifocus:%s" path)
  (shell-command (concat "open \"omnifocus:" path "\"")))

(defun alpo/org-mailplane-open-link (path)
  "Open a Mailplane url scheme"
  (let ((escpath (replace-regexp-in-string " " "%20" path)))
    (message "Open Mailplane link: mailplane:%s" escpath)
    (shell-command (concat "open \"mailplane:" escpath "\""))))

(defun alpo/org-2do-open-link (path)
  "Open a 2Do url scheme"
  (let ((escpath (replace-regexp-in-string " " "%20" path)))
    (message "Open 2Do link: twodo:%s" escpath)
    (shell-command (concat "open \"twodo:" escpath "\""))))
;; Load local customizations (local to the computer)
;; (when (file-exists-p "~/local.el")
;;   (load "~/local.el")))
)
;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (counsel smartparens flycheck magit with-editor f pug-mode yapfify yaml-mode xkcd ws-butler window-numbering which-key wgrep web-mode web-beautify volatile-highlights uuidgen use-package toc-org tagedit swift-mode spacemacs-theme spaceline smex smeargle slim-mode scss-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restart-emacs request rbenv rake rainbow-delimiters quelpa pyvenv pytest pyenv-mode py-isort popwin pip-requirements phpunit phpcbf php-extras php-auto-yasnippets persp-mode paradox orgit org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets open-junk-file omnisharp noflet neotree move-text mmm-mode material-theme markdown-toc magit-gitflow magit-gh-pulls macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode keyfreq key-chord json-mode js2-refactor js-doc jade-mode ivy-hydra info+ indent-guide ido-vertical-mode hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers highlight-indentation help-fns+ helm-make google-translate golden-ratio go-eldoc gnuplot gitignore-mode github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md focus flyspell-correct-ivy flx-ido flatui-theme fish-mode fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-snipe evil-search-highlight-persist evil-numbers evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-commentary evil-args evil-anzu eval-sexp-fu ensime emmet-mode elisp-slime-nav eclim dumb-jump disaster diff-hl deft define-word cython-mode counsel-projectile column-enforce-mode cmake-mode clean-aindent-mode clang-format chruby bundler auto-highlight-symbol auto-dictionary auto-compile anaconda-mode alect-themes aggressive-indent adoc-mode adaptive-wrap ace-window ace-link))))
