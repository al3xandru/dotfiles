
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

          ;; ("W" "Weekly planning"
          ;;  entry (file+datetree alpo-org-work-file)
          ;;  (file "~/Dropbox/Dox/mydox/90-weeklyplanning.org.template"))

          ("h" "New hire"
           entry (file+olp alpo-org-work-file "Projects" "PRJ Hire")
           (file "~/Dropbox/Dox/mydox/90-newhire.org.template"))

         
          ("m" "Meeting"
           entry (file+datetree alpo-org-work-file)
           "* MEETING %^{Description} (%U) :for_work:meeting:\n:PROPERTIES:\n:CREATED: %U\n:PEOPLE:\n:RECORDING:\n:END:\n%?")

          ("p" "Project"
           entry (file+olp org-default-notes-file "Projects")
           "* PRJ %? [%]\n** NEXT Write down what is the purpose or what are the goals/objectives for this project\n** TODO Write down what are the deliverables or what does it mean to be done\n** TODO Brainstorm initial set of tasks\n** TODO Organize tasks\n** TODO Identify next action")

          ("v" "Trip/vacation checklist"
           entry (file+datetree org-default-notes-file)
           (file  "~/Dropbox/Dox/mydox/90-trip.org.template"))

          
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

          
          ;; ("L" "Library")
          ;; ("Lp" "Pick up book"
          ;;  entry (file+olp alpo-org-tickler-file "Calendar")
          ;;  "* TODO Pick up book from library \"%^{Book}\" :#me:@library:\nDEADLINE: %^T")

          ;; ("Lr" "Return book to library"
          ;;  entry (file+olp alpo-org-tickler-file "Calendar")
          ;;  "* TODO Return book to library \"%^{Book}\" :#me:@library:\nDEADLINE: %^T")

          ;; ("Lm" "Return magazines to library"
          ;;  entry (file+olp alpo-org-tickler-file "Calendar")
          ;;  "* TODO Return to library %^{How many magazines} magazines (%^{What magazines}) :#me:@library:\nDEADLINE: %^T")
          ;; ;;; end "L"
 
 
          ;; ("M" "Movies")
          ;; ("Mn" "Movie in theather"
          ;;  entry (file+olp alpo-org-someday-file "Movies in theater")
          ;;  "* SOMEDAY When & where can we/I see movie \"[[https://www.google.com/search?hl=en&q=showtimes+san+francisco+%\\1][%^{Movie}]]\" :#me:@cinema:")
          
          ;; ("Mw" "Movie wishlist"
          ;;  entry (file alpo-org-someday-file)
          ;;  "* SOMEDAY Check out %^{Kind|movie|TV series} \"%^{Title}\"%? :#me:")
          
          ;; ("Md" "Movie offline"
          ;;  entry (file alpo-org-someday-file)
          ;;  "* SOMEDAY Check availability of movie \"%^{Title}\" :#me:")
          ;; ;;; end "M"
          

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

         ("w" "Weekend agenda with NEXT, on HOLD, and INBOX sections for daily use"
           ((agenda "My agenda"
                    ((org-agenda-entry-types '(:deadline :scheduled :timestamp :ts))
                     (org-agenda-files (mapcar (lambda (f) (concat org-directory f)) (list "11-inbox.org"
                                                                                           "12-mlo.org"
                                                                                           "13-tickler.org"
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
                   (org-agenda-sorting-strategy '(todo-state-up priority-down))
                   (org-agenda-files (mapcar (lambda (f) (concat org-directory f)) (list "11-inbox.org"
                                                                                         "12-mlo.org"
                                                                                         "13-tickler.org"
                                                                                         "19-maybes.org")))))

            (todo "WAIT|HOLD|WIP"
                  ((org-agenda-overriding-header "Waiting")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline))
                   (org-agenda-files (mapcar (lambda (f) (concat org-directory f)) (list "11-inbox.org"
                                                                                         "12-mlo.org"
                                                                                         "13-tickler.org"
                                                                                         "19-maybes.org")))))


            (agenda "Weekly agenda"
                    ((org-agenda-entry-types '(:deadline :scheduled :timestamp))
                     (org-agenda-files (mapcar (lambda (f) (concat org-directory f)) (list "11-inbox.org"
                                                                                           "12-mlo.org"
                                                                                           "13-tickler.org"
                                                                                           "19-maybes.org")))
                     (org-agenda-span 'week)
                     (org-deadline-warning-days 3)
                     (org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))))
            
            (todo "TODO|NEXT|SOMEDAY|DELEGATE"
                  ((org-agenda-overriding-header "Inbox")
                   (org-agenda-files (list (concat org-directory "11-inbox.org")))))
            )

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
                   (org-agenda-span 7)
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
