;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(setq gc-cons-threshold (* 128 1024 1024)
      gc-cons-percentage 0.6
      auto-window-vscroll nil)
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold (* 20 1024 1024)
                           gc-cons-percentage 0.1)
            (garbage-collect)) t)

;; General settings

;;; user details
(setq user-full-name "alpo"
      user-mail-address "alex@mypopescu.com")

;;; allow access from emacsclient
(require 'server)
(unless (server-running-p)
  (server-start))

;;; yes-or-no: https://www.reddit.com/r/emacs/comments/4fzgdn/what_difference_there_is_between_defalias_and_fset/
;;; (fset 'yes-or-no-p 'y-or-n-p)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq use-dialog-box nil)

;;; display
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq default-frame-alist
      '((width . 111)
        (height . 60)
        ;; (cursor-color . "#ffd700")
        (cursor-color . "#28c8FA")
        (ns-transparent-titlebar . t)))
        ;; (ns-appearance . dark)))
;; (when (member "MonoLisa" (font-family-list))
;;   (add-to-list 'default-frame-alist '(font .  "MonoLisa-13"))
;;   (set-face-attribute 'default t :font "MonoLisa-13"))
;; (when (member "mononoki" (font-family-list))
;;   (add-to-list 'default-frame-alist '(font . "mononoki-12"))
;;   (set-face-attribute 'default t :font "mononoki-12"))
;; (when (member "Operator Mono" (font-family-list))
;;   (add-to-list 'default-frame-alist '(font .  "Operator Mono-13"))
;;   (set-face-attribute 'default t :font "Operator Mono-13"))
;; (when (member "PragmataPro Mono" (font-family-list))
;;   (add-to-list 'default-frame-alist '(font .  "PragmataPro Mono-13"))
;;   (set-face-attribute 'default t :font "PragmataPro Mono-13"))
(when (member "Go Mono" (font-family-list))
  (add-to-list 'default-frame-alist '(font . "Go Mono-13"))
  (set-face-attribute 'default t :font "Go Mono-13"))
(when (member "Iosevka Fixed Slab" (font-family-list))
  (add-to-list 'default-frame-alist '(font . "Iosevka Fixed Slab-13"))
  (set-face-attribute 'default t :font "Iosevka Fixed Slab-13"))

;;;; scrolling
(setq scroll-margin 0
      scroll-conservatively 10000
      scroll-preserve-screen-position t)

(blink-cursor-mode 0)
(setq ring-bell-function 'ignore)


;;; splash screen
(setq inhibit-splash-screen t
      inhibit-startup-message t
      initial-scratch-message nil)
;; open the Bookmark list on startup
;; initial-buffer-choice (lambda () (bookmark-bmenu-list)
;;                               (switch-to-buffer "*Bookmark List*")))

;; Files
;;; Sessions
(desktop-save-mode 1)
;;; Symlinks
(setq vc-follow-symlinks t)

;;; Backups
(setq backup-directory-alist `((".*" . "~/.emacs.d/.backups"))
      ;; disable lockfiles .#
      create-lockfiles nil
      version-control t
      ;; uncomment next line for disabling backup files
      ;; make-backup-files nil
      )

(setq dired-use-ls-dired nil
      diff-hl-side 'left)

;;; auto-refresh
(global-auto-revert-mode 1)

;; Editor
(setq-default indicate-empty-lines t
              line-spacing 0.1
              scroll-margin 5)

(add-hook 'text-mode-hook #'flyspell-mode)

;;; utf-8
(setq buffer-file-coding-system 'utf-8
      file-name-coding-system 'utf-8
      locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-terminal-coding-system 'utf-8)

;;; indentation
(setq-default indent-tabs-mode nil
              tab-width 4
              evil-shift-width 4
              standard-indent 4)
(electric-indent-mode t)

;;; parenthesis
(electric-pair-mode 1)
(show-paren-mode 1)
(setq show-paren-delay 0.2)

;;; line numbers
(global-display-line-numbers-mode)


;;; highlight line
(global-hl-line-mode t)

;; Better bindings
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Mode-Line-Variables.html
;; http://www.hollenback.net/index.php/EmacsModeLine
(setq evil-mode-line-format '(after .  mode-line-front-space)
      column-number-mode t
      column-number-indicator-zero-based nil)

(setq-default mode-line-format
              '("%e"
                mode-line-front-space
                evil-mode-line-tag
                mode-line-frame-identification
                mode-line-buffer-identification
                " "
                mode-line-modified
                " ["
                (vc-mode vc-mode)
                " ][ "
                mode-name
                " ::"
                minor-mode-alist
                " ] "
                "   "
                mode-line-position
                mode-line-mule-info
                mode-line-misc-info
                mode-line-end-spaces))

;; (setq ido-enable-flex-matching t
;;       ido-everywhere t)
;; (ido-mode 1)
;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(progn
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (when (file-exists-p custom-file)
    (load custom-file)))

;; Load local customizations (local to the computer)
;; (when (file-exists-p "~/local.el")
;;   (load "~/local.el")))

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
(global-set-key (kbd "C-h R") #'alpo/describe-random-interactive-function)

(defun alpo/show-file-name ()
  "Show the full path file name in the minibuffer"
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name)))
(global-set-key (kbd "C-c z") #'alpo/show-file-name)

(defun alpo/new-scratch-file ()
  (interactive)
  (let ((new-file-name (file-name-concat "~/Desktop" (concat (format-time-string "%Y%m%d-%H%M") ".md"))))
    (make-empty-file new-file-name)
    (switch-to-buffer (find-file-noselect new-file-name))))
(global-set-key (kbd "C-c C-f") #'alpo/new-scratch-file)

;;; zap-up-to-char instead of zap-to-char https://www.emacswiki.org/emacs/ZapUpToChar
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR
   (fn arg char)"
  'interactive)
(global-set-key (kbd "M-Z") 'zap-to-char)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;;; One Window Toggle
;;; https://github.com/kaushalmodi/.emacs.d/blob/95da6831a5faad7909960a7772493ae045e51003/setup-files/setup-windows-buffers.el#L473-L493
;;; https://www.reddit.com/r/emacs/comments/6xg310/zooming_in_and_out_of_a_window/
(defvar modi/toggle-one-window--buffer-name nil
  "Variable to store the name of the buffer for which the `modi/toggle-one-window'
function is called.")
(defvar modi/toggle-one-window--window-configuration nil
  "Variable to store the window configuration before `modi/toggle-one-window'
function was called.")
(defun modi/toggle-one-window (&optional force-one-window)
  "Toggles the frame state between deleting all windows other than
the current window and the windows state prior to that."
  (interactive "P")
  (if (or (null (one-window-p))
          force-one-window)
      (progn
        (setq modi/toggle-one-window--buffer-name (buffer-name))
        (setq modi/toggle-one-window--window-configuration (current-window-configuration))
        (delete-other-windows))
    (progn
      (when modi/toggle-one-window--buffer-name
        (set-window-configuration modi/toggle-one-window--window-configuration)
        (switch-to-buffer modi/toggle-one-window--buffer-name)))))
(global-set-key (kbd "C-x C-z") #'modi/toggle-one-window)
;; ------------
;; Dependencies
;; ------------

(require 'package)
;; (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
;; (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

(setq gnutls-verify-error t
      gnutls-min-prime-bits 2048)

;(if (< emacs-major-version 29)
;    (package-initialize))
(package-initialize)

(unless (package-installed-p 'use-package)
  (progn
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)))

(require 'use-package)
(setq use-package-always-ensure t)

;;; --------
;;; Required: attempting simplified setup
;;; --------
;;; https://github.com/auwsmit/emacsconfig/blob/master/config.org#evil-mode
;; attempting simplified setup
(use-package evil
  :disabled
  :commands evil-mode
  :init
  (setq evil-default-state 'emacs
        evil-shift-width 4)
  :config
  (evil-mode t)
  (evil-set-initial-state 'org-mode 'emacs)
  (evil-set-initial-state 'undo-tree 'emacs)
  (evil-set-initial-state 'markdown-mode 'emacs)
  :bind (:map evil-normal-state-map
              ("j" . evil-next-visual-line)
              ("k" . evil-previous-visual-line)
              ("<up>" . evil-previous-visual-line)
              ("<down>" . evil-next-visual-line))
  :bind (:map evil-visual-state-map
              ("j" . evil-next-visual-line)
              ("k" . evil-previous-visual-line)))

(use-package evil-commentary
  :disabled
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-surround
  :disabled t
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-snipe
  :disabled t
  :after evil
  :init
  (setq evil-snipe-scope 'buffer
        evil-snipe-enable-highlight t)
  :config
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package markdown-mode
  :mode (("\\`README\\.md\\'" . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode))
  :init
  (setq markdown-command "~/bin/emarkdown"
        markdown-italic-underscore t
        markdown-reference-location 'end)
  :config
  (add-hook 'markdown-mode-hook (lambda () (set-fill-column 77)))
  (add-hook 'markdown-mode-hook #'flyspell-mode)
  (add-hook 'gfm-mode-hook #'flyspell-mode))


(use-package markdown-toc)

;; Copy reference to current point
;; Inspired by https://stackoverflow.com/questions/10681766/emacs-org-mode-textual-reference-to-a-fileline
(defun alpo/ref-at-point ()
  "Place on kill-ring details of the current point"
  (interactive)
  (kill-new (format "file:%s::%d" buffer-file-truename (line-number-at-pos)))
  (if (derived-mode-p 'org-mode)
      (let* ((header-info (assoc :headline (org-context)))
             (text (when header-info
                     (replace-regexp-in-string "\\ +$" ""
                                               (replace-regexp-in-string "\\(:[^:]+\\)+:$" ""
                                                                         (replace-regexp-in-string "\\*+\\ " ""
                                                                                                   (buffer-substring-no-properties (or (cadr header-info) (point-min))
                                                                  (or (caddr header-info) (point-max)))))))))
        (if (not text)
            (kill-new (format "re:task at line#%d" (line-number-at-pos)))
          (kill-new (format "re:%s" text))))
      (kill-new (format "ref %s#%d" (file-name-nondirectory (buffer-file-name)) (line-number-at-pos)))))
(global-set-key (kbd "C-x M-c") #'alpo/ref-at-point)

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

;; (load "~/.emacs.d/org.el")


(use-package rainbow-delimiters
  :hook (prog-mode .  rainbow-delimiters-mode))
  ;; :commands rainbow-delimiters-mode
  ;; :init (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))


;; https://github.com/sachac/.emacs.d/blob/gh-pages/Sacha.org#smartparens-mode
;; run-hooks: Autoloading file /Users/alex/Dropbox/workspaces/mine/miscellaneous/emacs.d/.emacs.d/elpa/smartparens-20180530.1401/smartparens.elc failed to define function smartparens
(use-package smartparens
  :hook (text-mode . smartparens-mode)
  :bind
  (("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-k" . sp-kill-sexp )
   ("C-M-d" . sp-down-sexp)
   ("C-M-u" . sp-backward-up-sexp)
   ("M-0" .  sp-forward-slurp-sexp)))
  ;; :commands smartparens-mode
  ;; :init (add-hook 'prog-mode-hook #'smartparens-mode))

;; https://github.com/auwsmit/emacsconfig/blob/master/config.org#show-available-key-bindings
(use-package which-key
  :defer 5
  :diminish
  :commands which-key-mode
  :config (which-key-mode))

;;; --------
;;; Optional
;;; --------
(use-package ace-jump-mode
  :disabled
  :init
  (setq ace-jump-mode-gray-background nil
   		ace-jump-mode-submode-list '(ace-jump-word-mode ace-jump-char-mode ace-jump-line-mode)
        ace-jump-mode-scope 'visible)
  :bind (("C-x SPC" . ace-jump-mode)			;; might be better with ace-jump-word-mode
         ("C-x x" . ace-jump-mode-pop-mark))
  :bind (:map evil-normal-state-map
              ("s" . ace-jump-mode)))

;; (use-package ace-window
;;   :bind ("C-x o" . ace-window)
;;   :config (custom-set-faces '(aw-leading-char-face ((t (:foreground "red" :weight normal :height 1.5))))))


(use-package avy
  :init
  (setq avy-timeout-seconds 0.4
        avy-style 'at
        avy-all-windows t
        avy-case-fold-search nil
        avy-highlight-first t)
  :bind (("C-;" .  avy-goto-char-timer)
         ("C-x j 1" . avy-goto-char)
         ("C-x j 2" . avy-goto-char-2)
         ("C-x j l" . avy-goto-line)
         ("C-x j w" . avy-goto-word-1))
  :bind (:map evil-normal-state-map
              ("s" . avy-goto-char-timer))
  :config (avy-setup-default))


(use-package expand-region
  :bind (("C-c C-e" . er/expand-region)
         ("C-c e" . er/expand-region)))

;; Bindings start with C-c C-w
(use-package eyebrowse
  :disabled
  ;; :ensure t
  :init
  (setq eyebrowse-keymap-prefix (kbd "C-c C-3")
        eyebrowse-new-workspace t)
  (global-unset-key (kbd "C-c C-w"))
  :config (eyebrowse-mode t))

;; `M-o' for extra actions on selection
(use-package ivy
  :disabled
  :init
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "
        ivy-wrap t)
  :config (ivy-mode 1)
  :bind (:map ivy-switch-buffer-map
              ("C-k" . ivy-switch-buffer-kill)))

(use-package counsel
  :disabled
  :after ivy
  :bind (("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-x j i" . counsel-imenu)
         ("C-x C-b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)))

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))


(use-package swiper
  :disabled
  :after ivy
  :bind (("C-x /" . swiper)
         ("C-s" .  swiper)))

(use-package ispell
  :no-require t
  :bind (("C-c i c" . ispell-comments-and-strings)
         ("C-c i d" . ispell-change-dictionary)
         ("C-c i k" . ispell-kill-ispell)
         ("C-c i m" . ispell-message)
         ("C-c i r" . ispell-region))
  :init (setq ispell-program-name "/usr/local/bin/aspell"))

 
(use-package persistent-scratch
  :ensure t
  :init (with-demoted-errors (persistent-scratch-restore))
  :config
  (defun alpo/scratch-buffer-p (&optional buffer)
    "Return non-nil if the BUFFER (defaults to current buffer) is a scratch buffer."
    (and (string-match-p "^\\*.*\\*$" (buffer-name buffer))
         (not (member (buffer-name buffer) '("*Backtrace*" "*Completions*" "*Help*" "*Ibuffer*" "*Messages*")))))
  (setq persistent-scratch-scratch-buffer-p-function #'alpo/scratch-buffer-p)
  (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode 1))

(use-package scratch
  :bind (("C-c s" . scratch)))

(use-package undo-tree
  :init
  (setq undo-tree-visualizer-diff t
        undo-tree-visualizer-timestamps t
        undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))
        ;; alternatively
        ;; undo-tree-auto-save-history nil
        )
  :config (global-undo-tree-mode))



;;; ------
;;; Themes
;;; ------
(use-package acme-theme :defer t :no-require t)
(use-package alect-themes    :defer t :no-require t)
(use-package anti-zenburn-theme :defer t :no-require t)
;; (use-package color-theme-github :defer t :no-require t)
(use-package darktooth-theme :defer t :no-require t)
;; (use-package doneburn-theme  :defer t :no-require t)
;; (use-package flatui-theme    :defer t :no-require t)
(use-package gruvbox-theme   :defer t :no-require t)
(use-package material-theme  :defer t :no-require t)
(use-package nova-theme      :defer t :no-require t)
(use-package plan9-theme     :defer t :no-require t)
(use-package subatomic-theme :defer t :no-require t)
(use-package twilight-bright-theme :defer t :no-require t)


;; Tips & Tricks

;; (when (version< emacs-version "26.1")
;;   (message "Emacs version %s < 26.1" emacs-version))

;; (if (version< emacs-version "26.1")
;;     (message "Emacs older than 26.1")
;;   (message "Emacs newer than 26.1"))

;;; theme
;;; light themes: leuven gruvbox-light-[hard|medium|soft] plan9 twilight-bright
;; (load-theme 'gruvbox-light-hard)
;; (load-theme 'plan9)
;; (load-theme 'dichromacy)
(load-theme 'acme)
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
