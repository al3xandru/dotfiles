;; -*- mode: emacs-lisp; tab-width: 2; evil-shift-width: 2; -*-
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
     spacemacs-ivy
     (spell-checking :variables spell-checking-enable-by-default nil)
     ;; UI
     ;; eyebrowse not needed in develop 0.106
     (ranger :variables ranger-show-preview t)
     themes-megapack
     theming
     ;; docs
     dash
     search-engine
     ;; git
     git
     github
     (version-control :variables
                      version-control-diff-tool 'diff-hl
                      version-control-global-margin t)
     ;; completion
     (auto-completion :variables
                      auto-completion-enable-help-tooltip t
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
     ;; twitter
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     evil-vimish-fold
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
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects bookmarks)
   ;; Number of recent files to show in the startup buffer. Ignored if
   ;; `dotspacemacs-startup-lists' doesn't include `recents'. (default 5)
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
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Operator Mono" ;; "PragmataPro Mono"h
                               :size  12
                               :weight normal
                               :width normal
                               :powerline-scale 1.1)
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
   ;; If non nil unicode symbols are displayed in the mode line.
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers t
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis t
   ;; If non nil the paste micro-state is enabled. While enabled pressing `p`
   ;; several times cycle between the kill ring content.
   dotspacemacs-enable-paste-micro-state t
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   dotspacemacs-search-tools '("ag" "grep" "ack")
   ;; Guide-key delay in seconds. The Guide-key is the popup buffer listing
   ;; the commands bound to the current keystrokes.
   dotspacemacs-which-key-delay 0.4
   ))


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
  ;;; Electric pairs
  (electric-indent-mode 1)
  (electric-pair-mode 1)
  ;; auto-completion key
  (global-set-key (kbd "S-SPC") 'company-complete)

  ;;; text-mode hooks
  (add-hook 'text-mode-hook 'auto-fill-mode)
  (add-hook 'text-mode-hook 'turn-on-flyspell)

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
  (defun alpo/autopairs-shortcut-jump ()
    (interactive)
    (backward-up-list 1)
    (forward-sexp 1))
  (define-key evil-insert-state-map (kbd "M-9") 'alpo/autopairs-shortcut-jump)
  (define-key evil-insert-state-map (kbd "M-0") 'evil-lisp-state-sp-forward-slurp-sexp)

  ;; avy
  (spacemacs/declare-prefix "s?" "avy-goto")
  ;; (spacemacs/set-leader-keys "SPC" 'evil-avy-goto-char)
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
  (spacemacs/set-leader-keys "F d" 'delete-frame)
  (spacemacs/set-leader-keys "F D" 'delete-other-frames)
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
    ;; (helm-find-files nil)
    (counsel-find-file nil)
    ;; (dired-other-frame (helm-current-directory))
    )
  (spacemacs/set-leader-keys "F p" 'alpo/new-frame-with-layout-for-project)
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
  (spacemacs/set-leader-keys "h r" 'alpo/describe-random-interactive-function)

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
  (evil-define-key 'insert markdown-mode-map (kbd "C-c C-d") 'alpo/ispell-complete-word)

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
  (spacemacs/set-leader-keys-for-major-mode 'markdown-mode "e" 'alpo/markdown-edit-mode)
  (spacemacs/set-leader-keys-for-major-mode 'markdown-mode "F" 'focus-mode)
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
  (setq python-shell-interpreter "python")

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

    (setq org-log-done t
          org-todo-keywords '((sequence "TODO(t)" "WANT(w)" "MUST(m)" "|" "DONE(d)" "FILED(f)" "SKIP(x@/!)")
                              (sequence "WIPR(p)" "WAIT(s@/!)")))

    (setq org-todo-keyword-faces '(("MUST" . (:foreground "#fe2500" :weight bold))
                                   ("TODO" . (:foreground "#cd4f39"))
                                   ("WANT" . (:foreground "#cc1b00" :weight bold :slant italic))
                                   ("WISH" . (:foreground "#cd4f39" :slant italic))
                                   ("WIPR" . (:foreground "#6495ed" :weight bold))
                                   ("FILED" . (:foreground "#698b69" :slant italic))
                                   ("DONE" . (:foreground "#698b69"))
                                   ("SKIP" . (:foreground "#ffd39b" :weight normal :strike-through t))))

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
           "+ %? %U\n  %c")))

    (setq org-agenda-span 14
          org-agenda-start-on-weekday 1)

    ;; http://orgmode.org/manual/Adding-hyperlink-types.html#Adding-hyperlink-types
    (org-add-link-type "twodo" 'alpo/org-2do-open-link)
    (org-add-link-type "mailplane" 'alpo/org-mailplane-open-link)
    (org-add-link-type "omnifocus" 'alpo/org-omnifocus-open-link))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((evil-shift-width . 2)))))
