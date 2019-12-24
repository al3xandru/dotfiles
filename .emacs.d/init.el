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
(when (member "Anka/Coder Narrow" (font-family-list))
  (add-to-list 'default-frame-alist '(font .  "Anka/Coder Narrow-13"))
  (set-face-attribute 'default t :font "Anka/Coder Narrow-13"))

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
;; (if (version< emacs-version "26.1")
;; (if (< emacs-major-version 26)
;;     (progn
;;       (global-linum-mode t)
;;       (let ((inhibit-message t))
;;         (message "enable line numbers: global-linum-mode")))
;;   (progn
;;     (setq display-line-numers-type t) ;; 'relative 'visual nil
;;     (global-display-line-numbers-mode)
;;     (let ((inhibit-message t))
;;       (message "enable line numbers: display-line-numbers"))))

;; Previous else branch:
;; version 1
;; (setq-default display-line-numbers t)
;; version 2:
;; (custom-set-variables '(display-line-numbers-type t)) ;; 'relative
;; (global-display-line-numbers-mode)


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
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))

(setq gnutls-verify-error t
      gnutls-min-prime-bits 2048)

(if (< emacs-major-version 27)
    (package-initialize))

(unless (package-installed-p 'use-package)
  (progn
    (unless package-archive-contents
      (package-refresh-contents))
    (package-install 'use-package)))

(require 'use-package)
(setq use-package-always-ensure t)

;;; --------
;;; Required
;;; --------
;;; https://github.com/auwsmit/emacsconfig/blob/master/config.org#evil-mode
(use-package evil
  :commands evil-mode
  :init
  (setq evil-default-state 'emacs
        evil-shift-width 4)
  :config
  (evil-mode t)
  (evil-set-initial-state 'org-mode 'emacs)
  (evil-set-initial-state 'undo-tree 'emacs)
  (evil-set-initial-state 'markdown-mode 'normal)
  :bind (:map evil-normal-state-map
              ("j" . evil-next-visual-line)
              ("k" . evil-previous-visual-line)
              ("<up>" . evil-previous-visual-line)
              ("<down>" . evil-next-visual-line))
  :bind (:map evil-visual-state-map
              ("j" . evil-next-visual-line)
              ("k" . evil-previous-visual-line)))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode))

(use-package evil-surround
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
  (add-hook 'markdown-mode-hook (lambda () (set-fill-column 74)))
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


(use-package org
  :ensure org-plus-contrib 			;; https://github.com/jwiegley/use-package/issues/597
  :bind (("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         :map org-mode-map
         ("C-c l" . org-store-link)
         ("C-c M-c" .  alpo/org-retrieve-url-at-point))
  :config
  (add-to-list 'org-modules 'org-checklist)
  (add-to-list 'org-modules 'org-habit)
  (add-to-list 'org-modules 'org-protocol)
  (setq org-hide-leading-stars t
        org-startup-indented t
        org-use-speed-commands t)

  ;; http://endlessparentheses.com/changing-the-org-mode-ellipsis.html#disqus_thread
  ;; (setq org-ellipsis "…")

  (setq org-directory "~/Dropbox/Dox/mydox/"
        org-default-notes-file (concat org-directory "12-mlo.org")
        org-agenda-files (mapcar (lambda (f) (concat org-directory f)) (list "11-inbox.org"
                                                                             "12-mlo.org"
                                                                             "13-tickler.org"
                                                                             "14-mho.org"
                                                                             "15-wpo.org"
                                                                             "19-maybes.org")))
  
  (setq alpo-org-readlater-file (concat org-directory "18-reads.org")
        alpo-org-tickler-file (concat org-directory "13-tickler.org")
        alpo-org-work-file (concat org-directory "15-wpo.org")
        alpo-org-someday-file (concat org-directory "19-maybes.org"))

  ;; check https://github.com/pisemsky/dotfiles/blob/b287adf0278ffc55e4fa47ee083c36e72c6ab751/.emacs.d/lisp/init-org.el
  (setq org-refile-targets '((org-agenda-files :maxlevel .  4)
                             ("99-org-cheatsheet.org" :maxlevel .  8))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes t)

  (setq org-archive-location "orgdata/archive/%s.archive::"
        org-attach-directory "orgdata/attachments/")

  ;; MobileOrg https://orgmode.org/org.html#MobileOrg
  (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg"
        org-mobile-inbox-for-pull "~/Dropbox/Dox/mydox/11-inbox.org")

  ;; (add-hook 'org-mode-hook
  ;;           (lambda ()
  ;;             (add-hook 'after-save-hook 'alpo/org-mobile-push-on-save nil 'make-local)))

  (setq org-log-done t
        org-log-into-drawer "LOGBOOK")
  
  (setq org-enforce-todo-dependencies t
        org-track-ordered-property-with-tag t
        org-agenda-dim-blocked-tasks t)

  (setq org-agenda-span 14
        org-agenda-start-on-weekday nil) ;; nil: begin on current day, t: begin Monday

  (org-babel-do-load-languages 'org-babel-load-languages '((emacs-lisp . t)
                                                           (shell .  t)
                                                           (dot .  t)
                                                           (org .  t)))
  (setq org-export-coding-system 'utf-8)

  ;; org-habit
  (setq org-habit-preceding-days 3
        org-habit-following-days 3
        org-habit-graph-column 80)

  ;; enable on a per-file basis #+STARTUP: customtime
  ;; (setq org-display-custom-times t
  ;;       org-time-stamp-custom-formats '("<%a,%b.%d>" . "<%a,%b.%d %H:%M>"))
  
  (setq org-tag-alist '(("@PRJ" . nil)
                        (:startgroup)
                        ("for_me" . ?9)
                        ("for_work" . ?0) 
                        (:endgroup)

                        (:startgroup)
                        ("aor_manageup" .  ?1)
                        ("aor_hiring" .  ?2)
                        ("aor_projmgmt" .  ?3)
                        ("aor_teammgmt" .  ?4)
                        ("aor_learning" .  ?5)
                        (:endgroup)
                        
                        (:startgrouptag)
                        ("who" .  ?W)
                        (:grouptags)
                        ("carter_shanklin" . ?c)
                        ("prashant_jha" . ?p)
                        ("greg_pavlik" . ?g)
                        ("dan_bradford" . nil)

                        ("balint_papp" .  nil)
                        ("dimitris_tzannetos" . ?t)
                        ("edwin_biemond" . ?b)
                        ("gyorgy_geiszter" . nil)
                        ("iris_chen" . ?h)
                        ("jeban_kanagarajan" . ?k)
                        ("jon_maron" . ?m)
                        ("kevin_minder" . ?i)
                        ("manish_jain" . ?j)
                        ("peter_gordos" . ?g)
                        ("robert_levas" . ?l)
                        ("thomas_lau" . ?a)
                        ("zoltan_koszegi" . nil)
                        ;; (:newline)
                        ("durgasuresh_kagitha". nil)
                        ("sonali_birari" . nil)
                        (:endgrouptag)

                        (:startgrouptag)
                        (:grouptags)
                        ("calendar" . ?C)
                        ("confluence" . ?X)
                        ("email" . ?E)
                        ("jira" . ?J)
                        ("meeting" . ?M)
                        ("org" . ?O)
                        ("phone" . ?P)
                        ("slack" . ?K)
                        ("vpn" . ?V)
                        ("zoom" . ?Z)
                        (:endgrouptag)

                        (:startgroup)
                        ("@office" . ?R)
                        ("@home" . ?H)
                        ("@bank" . ?B)
                        ("@library" . ?L)
                        ("@travel" . ?T)
                        ("@store" . ?S)
                        (:endgroup)
                        ))

  ;; Remember to document how a sequence works if it's not self-explanatory
  (setq org-todo-keywords '(
                            ;; my normal sequence:
                            ;; NEXT: the 
                            ;; DELEGATE/TASK: are tasks that I want to delegate
                            ;; SOMEDAY: tasks I could consider sometime in the future
                            ;; WAIT: I am waiting for something to be able to complete or proceed
                            (sequence "TODO(t)" "NEXT(n!/!)" "DELEGATE(k!/!)" "SOMEDAY(s)" "WAIT(w@/!)" "|" "DONE(d!)" "SKIP(x@/!)")
                            ;; separate sequence for team tasks
                            ;; DELEGATE/TASK: a task that I want to delegate.  It is *not* yet delegated
                            ;; WIP: a task that was assigned and I'm waiting on its completion
                            (sequence "DELEGATE(k!/!)" "WIP(p!/!)" "TODO(t@/!)"  "|" "DONE(d!)" "SKIP(x@/!)")))


  ;; (setq org-todo-keyword-faces '(("TODO" . (:foreground "#dc752f" :weight normal))
  ;;                                ("NEXT" . (:foreground "#c61b6e" :weight bold))
  ;;                                ("WAIT" . (:foreground "#5859b7" :slant italic))
  ;;                                ("HOLD" . (:foreground "#2075c7" :slant italic))
  ;;                                ("DONE" . (:foreground "#52676f" :weight normal :strike-through t))
  ;;                                ("SKIP" . (:foreground "#465a61" :weight normal :slant italic :strike-through t))
  ;;                                ("SOMEDAY" .  (:foreground "#a57705" :weight normal))
  ;;                                ("PRJ" .  (:foreground "#4f97d7" :weight bold :height 1.2))
  ;;                                ("MEETING" .  (:foreground "#83AFE5" :weight normal))))
  (setq org-todo-keyword-faces '(("TODO" . (:weight normal))
                                 ("NEXT" . (:weight bold))
                                 ("DELEGATE" . (:weight bold))
                                 ("WAIT" . (:slant italic))
                                 ("WIP" . (:slant italic))
                                 ("HOLD" . (:slant italic))
                                 ("DONE" . (:weight normal :strike-through t))
                                 ("SKIP" . (:weight normal :slant italic :strike-through t))
                                 ("SOMEDAY" . (:weight normal))
                                 ("PRJ" . (:weight bold :height 1.2))
                                 ("MEETING" . (:weight normal))))

  (setq org-capture-templates
        '(("t" "* TODO"
           entry (file+datetree org-default-notes-file)
           "* TODO %?\n:PROPERTIES:\n:CREATED: %u\n:END:\n%i")

          ("e" "* TODO Respond to email"
           entry (file+datetree org-default-notes-file)
           "* TODO Respond to email subject:(%^{mail|zol[mp]}) from:(%^{name|none}) :email:\n:PROPERTIES:\n:CREATED: %u\n:END:")

          ("T" "* TODO for_work"
           entry (file+datetree alpo-org-work-file)
           "* TODO %? :for_work:\n:PROPERTIES:\n:CREATED: %u\n:END:\n%i")

          ("E" "* TODO Respond to email for_work"
           entry (file+datetree alpo-org-work-file)
           "* TODO Respond to email subject:(%^{mail|zol[mp]}) from:(%^{name|none}) :for_work:email:\n:PROPERTIES:\n:CREATED: %u\n:END:")

          ("W" "Weekly planning"
           entry (file+datetree alpo-org-work-file)
           (file "~/Dropbox/Dox/mydox/90-weeklyplanning.template.org"))
          
          ("m" "Meeting"
           entry (file+datetree alpo-org-work-file)
           "* MEETING %^{Description} (%U) :for_work:meeting:\n:PROPERTIES:\n:CREATED: %U\n:PEOPLE:\n:RECORDING:\n:END:\n%?")

          ("p" "Project"
           entry (file+olp org-default-notes-file "Projects")
           "* PRJ %? [%]\n** NEXT Write down what is the purpose or what are the goals/objectives for this project\n** TODO Write down what are the deliverables or what does it mean to be done\n** TODO Brainstorm initial set of tasks\n** TODO Organize tasks\n** TODO Identify next action")

          ("v" "Trip/vacation checklist"
           entry (file+datetree org-default-notes-file)
           (file  "~/Dropbox/Dox/mydox/90-trip.template.org"))

          
          ("x" "Using clipboard...")
          ("xu" "* TODO with URL in clipboard"
           entry (file+datetree org-default-notes-file)
           "* TODO %^{Action|Read|Check|Bookmark|Save|Download|Watch} \"[[%c][%?]]\"\n:PROPERTIES:\n:CREATED: %u\n:END:")

          ("xs"
           "* SOMEDAY (Action) with URL in clipboard"
           entry (file+olp alpo-org-someday-file "Read later")
           "* SOMEDAY %^{Action|Read|Check|Bookmark|Save|Download|Watch} \"[[%c][%?]]\"\n:PROPERTIES:\n:CREATED: %u\n:END:")

          ("xt" "* TODO with clipboard"
           entry (file+datetree org-default-notes-file)
           "* TODO %?\n%c\n:PROPERTIES:\n:CREATED: %u\n:END:\n%i")
          
          ("xn" "Note with clipboard"
           item (file+datetree org-default-notes-file)
           "+ %c [%u]")
          ;;; end "x"

          
          ("y" "Using reference...")
          ("yt" "* TODO with ref"
           entry (file+datetree org-default-notes-file)
           "* TODO %a\n:PROPERTIES:\n:CREATED: %u\n:END:\n%i")
          
          ("yn" "Note with ref"
           item (file+datetree org-default-notes-file)
           "+ %a [%u]\n%i")
          ;;; end "y"

          
          ("L" "Library")
          ("Lp" "Pick up book"
           entry (file+olp alpo-org-tickler-file "Calendar")
           "* TODO Pick up book from library \"%^{Book}\" :#me:@library:\nDEADLINE: %^T")

          ("Lr" "Return book to library"
           entry (file+olp alpo-org-tickler-file "Calendar")
           "* TODO Return book to library \"%^{Book}\" :#me:@library:\nDEADLINE: %^T")

          ("Lm" "Return magazines to library"
           entry (file+olp alpo-org-tickler-file "Calendar")
           "* TODO Return to library %^{How many magazines} magazines (%^{What magazines}) :#me:@library:\nDEADLINE: %^T")
          ;;; end "L"
 
 
          ("M" "Movies")
          ("Mn" "Movie in theather"
           entry (file+olp alpo-org-someday-file "Movies in theater")
           "* SOMEDAY When & where can we/I see movie \"[[https://www.google.com/search?hl=en&q=showtimes+san+francisco+%\\1][%^{Movie}]]\" :#me:@cinema:")
          
          ("Mw" "Movie wishlist"
           entry (file alpo-org-someday-file)
           "* SOMEDAY Check out %^{Kind|movie|TV series} \"%^{Title}\"%? :#me:")
          
          ("Md" "Movie offline"
           entry (file alpo-org-someday-file)
           "* SOMEDAY Check availability of movie \"%^{Title}\" :#me:")
          ;;; end "M"
          

          ;;; automation
          ("X" "TODO Check URL"
           entry (file+datetree org-default-notes-file)
           "* TODO Check link '[[%:link][%:description]]'\n:PROPERTIES:\n:CREATED: %u\n:END:\n%i")

          ("xx" "TODO with email URL"
            entry (file+datetree alpo-org-work-file)
           "* TODO %^{Action|Check|Respond} (%U) :for_work:\n:PROPERTIES:\n:CREATED: %U\n:END:\n%?")

          ("*"
           "Random todo mainly for automation"
           entry
           (file+datetree alpo-org-work-file)
           "* %?\n:PROPERTIES:\n:CREATED: %u\n:END:\n%i")
          ))


  (setq org-agenda-custom-commands
        '(("c" . "Custom agenda views")
          ("d" "Weekly agenda with NEXT, on HOLD, and INBOX sections for daily use"
           ((agenda "Today agenda"
                    ((org-agenda-entry-types '(:deadline :scheduled :timestamp :ts))
                     (org-agenda-files (mapcar (lambda (f) (concat org-directory f)) (list "11-inbox.org"
                                                                                           "12-mlo.org"
                                                                                           "13-tickler.org"
                                                                                           "15-wpo.org"
                                                                                           "19-maybes.org")))
                     (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                     (org-agenda-span 'day)
                     (org-deadline-warning-days 0)))

            (agenda "Habits"
                  ((org-agenda-overriding-header "Habits:")
                   (org-agenda-files (list (concat org-directory "14-mho.org")))
                   (org-agenda-span 'day)
                   (org-deadline-warning-days 0)))

            (todo "NEXT|DELEGATE"
                  ((org-agenda-overriding-header "Next actions:")
                   (org-agenda-sorting-strategy '(todo-state-up priority-down))))

            (todo "WAIT|HOLD|WIP"
                  ((org-agenda-overriding-header "Waiting")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))))


            (agenda "Weekly agenda"
                    ((org-agenda-entry-types '(:deadline :scheduled :timestamp))
                     (org-agenda-files (mapcar (lambda (f) (concat org-directory f)) (list "11-inbox.org"
                                                                                           "12-mlo.org"
                                                                                           "13-tickler.org"
                                                                                           "15-wpo.org"
                                                                                           "19-maybes.org")))
                     (org-agenda-span 'week)
                     (org-deadline-warning-days 3)
                     (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))))
            
            (todo "TODO|NEXT|SOMEDAY|DELEGATE"
                  ((org-agenda-overriding-header "Inbox")
                   (org-agenda-files (list (concat org-directory "11-inbox.org")))))
            )

           ;; Uncomment next 2 lines to change the format of the view
           ;; ((org-columns-default-format "%CATEGORY %5TODO %1PRIORITY %20SCHEDULED %20DEADLINE %ITEM")
           ;;  (org-agenda-view-columns-initially t))
           )
                    
          ("b" "Backlog with due now (DEADLINE + NEXT), available now, and due soon"
           ((tags-todo "DEADLINE<\"<+1d>\"/!TODO|NEXT|WAIT|DELEGATE|WIP"
                       ((org-agenda-overriding-header "Urgent deadlines (today and past):")
                        (org-agenda-sorting-strategy '(habit-down deadline-down priority-down))))

            (todo "NEXT|DELEGATE"
                  ((org-agenda-overriding-header "My next actions:")
                   (org-agenda-sorting-strategy '(todo-state-up priority-down))))
            
            ;; (tags-todo "TIMESTAMP<\"<+1d>\"|SCHEDULED<\"<+1d>\"/!TODO|NEXT|WAIT|DELEGATE|WIP"
            (tags-todo "TIMESTAMP<\"<+1d>\"|SCHEDULED<\"<+1d>\""
                       ((org-agenda-overriding-header "Available now:")
                        (org-agenda-sorting-strategy '(habit-down ts-down scheduled-down priority-down))
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottimestamp 'notscheduled 'notdeadline))
                        ;; (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottimestamp 'deadline))
                        ))

            (tags-todo "+DEADLINE>\".\"+DEADLINE<=\"<+1w>\"/!TODO|NEXT|WAIT|DELEGATE|WIP"
                       ((org-agenda-overriding-header "Deadlines in next 7 days:")
                        (org-agenda-sorting-strategy '(habit-down deadline-up priority-down))))

            ;; (tags-todo "+TIMESTAMP>\"<0d>\"+TIMESTAMP<\"<+8d>\"|+SCHEDULED>\"<0d>\"+SCHEDULED<\"<+8d>\"/!TODO|NEXT|WAIT"
            (tags-todo "+TIMESTAMP>\".\"+TIMESTAMP<\"<+8d>\"|+SCHEDULED>\".\"+SCHEDULED<\"<+8d>\"/!TODO|NEXT|WAIT|DELEGATE|WIP"
                       ((org-agenda-overriding-header "Planned for next week:")
                        (org-agenda-sorting-strategy '(habit-down timestamp-up priority-down))
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))))

            ;; keeping custom view focused
            ;; (tags-todo "-DEADLINE<=\"<+1w>\"&-SCHEDULED<=\"<+1w>\"/!TODO|NEXT"
            ;;            ((org-agenda-overriding-header "Backlog")
            ;;             (org-agenda-sorting-strategy '(timestamp-up priority-down))))
            )
           
           ((org-agenda-files  (mapcar (lambda (f) (concat org-directory f)) (list "11-inbox.org"
                                                                                   "12-mlo.org"
                                                                                   "13-tickler.org"
                                                                                   "15-wpo.org"
                                                                                   "19-maybes.org")))
           ))

          ("h" "Habits"
           ((agenda "Habits"
                  ((org-agenda-files (list (concat org-directory "14-mho.org")))
                   (org-agenda-span 3)
                   (org-deadline-warning-days 0)))))
          
          ("Z" "Archive m CLOSED<\"<-1m>\""
           ((tags-todo "+CLOSED<\"<-1m>\""             ;; (todo "DONE|SKIP"
                  ((org-agenda-overriding-header "Archive")
                   (org-tags-match-list-sublevels nil)
                   (org-agenda-sorting-strategy '(tsia-down)))))
           ;; ((org-agenda-files (mapcar (lambda (f) (concat org-directory f)) (list "11-inbox.org"
           ;;                                                                        "12-mlo.org"
           ;;                                                                        "13-tickler.org"
           ;;                                                                        "14-mho.org"
           ;;                                                                        "19-maybes.org"))))
           )

          ))


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
  (org-link-set-parameters
   "bear"
   :follow (lambda (path) (alpo/org-open-typed-link "bear" path))
   :face '(:foreground "#C8252B" :underline t)
   :help-echo "Open in Bear")
  ;; (org-add-link-type "twodo" #'alpo/org-2do-open-link)


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


  ;; Activate appointments so we get notifications
  ;; (appt-activate t)
  ;; This is at the end of my .emacs - so appointments are set up when Emacs starts
  (alpo/org-agenda-to-appt)
  ;; If we leave Emacs running overnight - reset the appointments one minute after midnight
  (run-at-time "00:01" (* 15 60) 'alpo/org-agenda-to-appt)
  ;; Rebuild the reminders everytime the agenda is displayed
  (add-hook 'org-finalize-agenda-hook 'alpo/org-agenda-to-appt 'append))

;; https://stackoverflow.com/a/31360779
(defvar alpo/org-mobile-push-timer nil)

(defun alpo/org-mobile-push-on-save ()
  "Used as 'after-save-hook to automatically push to org-mobile"
  (interactive)
  (let ((secs 10))
  (when (memq this-command '(save-buffer save-some-buffer))
    (when alpo/org-mobile-push-timer
      (cancel-timer alpo/org-mobile-push-timer))
    (setq alpo/org-mobile-push-timer
          (run-with-idle-timer
           (* 1 secs) nil (lambda ()
                            (org-mobile-push)
                            (alpo/message-in-buffer "org-mobile-push")))))))

(defun alpo/org-open-typed-link (type path)
  "Open an URL using a custom scheme"
  (let ((escpath (replace-regexp-in-string " " "%20" path)))
    (alpo/message-in-buffer "Open link: '%s:%s'" type escpath)
    (shell-command (format "open \"%s:%s\"" type escpath))))


(defun alpo/org-agenda-to-appt ()
  "Refresh appointments by firstly cleaning up the list."
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt) 
  (alpo/message-in-buffer "refresh org reminders"))


(defsubst alert-encode-string (str)
  (encode-coding-string str (keyboard-coding-system)))

(defsubst appt-delete-window()
  "Nothing. overwrite built-in func to avoid error: Error running timer ‘appt-delete-window’: (error \"No buffer named *appt-buf*\")")

(defun alpo/osx-appt-display (min-to-appt new-time msg)
  "Display macOS notifications"
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

    (alpo/message-in-buffer "appt: %s in %s minutes at %s" (nth i msg) (nth i min-to-appt) (nth i new-time))))
    ;; (message "appt: %s in %s minutes at %s" (nth i msg) (nth i min-to-appt) (nth i new-time))))

(defun alpo/message-in-buffer (format-string &rest args)
  "Log the message in the *Message* buffer supressing the minibuffer message"
  (interactive)
  (let ((inhibit-message t)
        (final-format-string (concat "[%s] " format-string))
        (args-list (cons (format-time-string "%H:%M%p" (current-time)) args)))
    (apply 'message final-format-string args-list)))


;; all this can become :config according to https://github.com/jwiegley/use-package/issues/453
;; (with-eval-after-load 'org )

;; (use-package org-alert)
(use-package org-bullets
  :after (org)
  :hook (org-mode .  org-bullets-mode)
  :init (setq org-bullets-bullet-list '("✸" "◉" "○" "◆" "▶")
              inhibit-compacting-font-caches t)) ;; https://github.com/sabof/org-bullets/issues/11#issuecomment-439228372a

(use-package org-download
  :ensure t
  :hook ((org-mode . org-download-enable)
         (dired-mode . org-download-enable))
  :config
  (setq org-download-method 'attach))

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

(use-package ace-window
  :bind ("C-x o" . ace-window)
  :config (custom-set-faces '(aw-leading-char-face ((t (:foreground "red" :weight normal :height 1.5))))))


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

(use-package counsel
  :after ivy
  :bind (("M-x" . counsel-M-x)
         ("M-y" . counsel-yank-pop)
         ("C-x j i" . counsel-imenu)
         ("C-x C-b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)))

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
  :init
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d "
        ivy-wrap t)
  :config (ivy-mode 1)
  :bind (:map ivy-switch-buffer-map
              ("C-k" . ivy-switch-buffer-kill)))

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))


(use-package swiper
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
  (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode 1))


(use-package undo-tree
  :init
  (setq undo-tree-visualizer-diff t
        undo-tree-visualizer-timestamps t)
  :config (global-undo-tree-mode))



;;; ------
;;; Themes
;;; ------
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
(load-theme 'plan9)
;; (load-theme 'dichromacy)

