;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ian Cole"
      user-mail-address "imcole@pm.me")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
(setq doom-font (font-spec :family "CaskaydiaCove Nerd Font" :size 14)
      doom-big-font (font-spec :family "CaskaydiaCove Nerd Font" :size 24)
      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font Mono" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(setq default-frame-alist
      (append (list
               '(fullscreen . fullheight)
               '(min-width  . 1)
               '(vertical-scroll-bars . nil)
               '(left-fringe    . 0)
               '(right-fringe   . 0)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0))))

(setq doom-emoji-fallback-font-families nil)

(add-to-list 'exec-path "/home/icole/.asdf/shims")

(setq org-duration-format (quote h:mm))
(setq org-agenda-files (quote ("~/org")))

;; (setq org-capture-templates
;;       `(("h" "Habit"
;;          entry (file "~/org/habits.org")
;;          "* TODO %? :Habit:\nSCHEDULED: <%<%Y-%m-%d %a .+1d>>\n:PROPERTIES:\n:CREATED: %U\n:STYLE: habit\n:REPEAT_TO_STATE: TODO\n:LOGGING: DONE(!)\n:ARCHIVE: %%s_archive::* Habits\n:END:\n%U\n"
;;          :kill-buffer t)
;;         ))

(bind-key "C-c C-y" 'org-todo-yesterday)

(cl-loop for file in '("/usr/local/bin/fish" "/usr/bin/fish")
         when (file-exists-p file)
         do (progn
              (setq shell-file-name file)
              (cl-return)))
(setenv "SHELL" shell-file-name)

(setq global-auto-revert-mode t)

(setq json-reformat:indent-width 2)

(setq auth-sources
    '((:source "~/.authinfo.gpg")))

(add-hook 'sh-mode-hook 'shfmt-on-save-mode)

(after! org
  (map! (:leader
         (:prefix "m"
          (:prefix "c"
           :desc "Start Pomodoro" "p" #'org-pomodoro
           ))))

  (add-to-list 'org-modules 'org-habit)
  (setq org-agenda-block-separator nil)
  (setq org-habit-following-days 1)
  (setq org-habit-show-habits t)
  (setq org-habit-preceding-days 30)
  (setq org-habit-show-habits-only-for-today t)
  (setq org-agenda-skip-scheduled-if-done t)
  (setq org-agenda-show-future-repeats nil)
  (setq org-agenda-todo-ignore-with-date t)
  (add-hook 'org-after-todo-state-change-hook #'org-save-all-org-buffers))

(after! mu4e
  (setq mu4e-root-maildir "~/.mail/Personal"
        mu4e-attachment-dir "~/Downloads"
        mu4e-sent-folder "/Personal/Sent"
        mu4e-drafts-folder "/Personal/Drafts"
        mu4e-trash-folder "/Personal/Trash"
        mu4e-refile-folder "/Personal/Archive"
        mu4e-use-fancy-chars t
        mu4e-view-show-images t
        shr-color-visible-luminance-min 80)

  (add-to-list 'mu4e-bookmarks
    '( :name  "Unread Today"
       :query "flag:unread AND NOT flag:trashed AND date:today..now"
       :key   ?o))

  ;; Get mail
  (setq mu4e-get-mail-command "mbsync personal"
        mu4e-change-filenames-when-moving t   ; needed for mbsync
        mu4e-update-interval 600)             ; update every 10 minutes

  ;; Send mail
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-auth-credentials "~/.authinfo.gpg" ;; Here I assume you encrypted the credentials
        smtpmail-smtp-server "127.0.0.1"
        smtpmail-smtp-service 1025
        smtpmail-stream-type 'starttls)

  (setq mail-user-agent 'mu4e-user-agent)
  (setq org-msg-options "html-postamble:nil H:5 num:nil ^:{} toc:nil author:nil email:nil \\n:t"
      org-msg-startup "hidestars indent inlineimages"
      org-msg-default-alternatives '((new . (text html))
                                     (reply-to-html . (text html))
                                     (reply-to-text . (text)))
      org-msg-convert-citation t)
  (org-msg-mode))

(setq json-reformat:indent-width 2)

(use-package company
  :hook (prog-mode . company-mode))


(after! gnutls
  (add-to-list 'gnutls-trustfiles
    (expand-file-name "~/.cert/protonmail.smtp.crt"))
)

;; Config copied from System Crafters 5 Org Roam Hacks video
;; https://systemcrafters.net/build-a-second-brain-in-emacs/5-org-roam-hacks/
(after! org-roam
  (map! (:leader
       (:prefix "n"
        :desc "Dalies map" "d" #'org-roam-dailies-map
        (:prefix "r"
         :desc "Insert immediate" "I" #'org-roam-node-insert-immediate
         :desc "Capture task" "t" #'my/org-roam-capture-task
         :desc "Capture inbox item" "b" #'my/org-roam-capture-inbox
         :desc "Find project" "p" #'my/org-roam-find-project))))


        (setq +org-roam-open-buffer-on-find-file nil)

        (defun org-roam-node-insert-immediate (arg &rest args)
        (interactive "P")
        (let ((args (push arg args))
                (org-roam-capture-templates (list (append (car org-roam-capture-templates)
                                                        '(:immediate-finish t)))))
        (apply #'org-roam-node-insert args)))

        (defun my/org-roam-filter-by-tag (tag-name)
        (lambda (node)
        (member tag-name (org-roam-node-tags node))))

        (defun my/org-roam-list-notes-by-tag (tag-name)
        (mapcar #'org-roam-node-file
                (seq-filter
                (my/org-roam-filter-by-tag tag-name)
                (org-roam-node-list))))

        (defun my/org-roam-refresh-agenda-list ()
        (interactive)
        (org-revert-all-org-buffers)
        (setq org-agenda-files (append (my/org-roam-list-notes-by-tag "Project") '("~/org"))))

        ;; Build the agenda list the first time for the session
        (my/org-roam-refresh-agenda-list)

        (defun my/org-roam-project-finalize-hook ()
        "Adds the captured project file to `org-agenda-files' if the
        capture was not aborted."
        ;; Remove the hook since it was added temporarily
        (remove-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

        ;; Add project file to the agenda list if the capture was confirmed
        (unless org-note-abort
        (with-current-buffer (org-capture-get :buffer)
        (add-to-list 'org-agenda-files (buffer-file-name)))))

        (defun my/org-roam-find-project ()
        (interactive)
        ;; Add the project file to the agenda after capture is finished
        (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

        ;; Select a project file to open, creating it if necessary
        (org-roam-node-find
        nil
        nil
        (my/org-roam-filter-by-tag "Project")
        :templates
        '(("p" "project" plain "* Goals\n\n%?\n\n* Tasks\n\n** TODO Add initial tasks\n\n* Dates\n\n"
        :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+category: ${title}\n#+filetags: Project")
        :unnarrowed t))))

        (defun my/org-roam-capture-inbox ()
        (interactive)
        (org-roam-capture- :node (org-roam-node-create)
                        :templates '(("i" "inbox" plain "* %?"
                                        :if-new (file+head "Inbox.org" "#+title: Inbox\n")))))

        (defun my/org-roam-capture-task ()
        (interactive)
        ;; Add the project file to the agenda after capture is finished
        (add-hook 'org-capture-after-finalize-hook #'my/org-roam-project-finalize-hook)

        ;; Capture the new task, creating the project file if necessary
        (org-roam-capture- :node (org-roam-node-read
                                nil
                                (my/org-roam-filter-by-tag "Project"))
                        :templates '(("p" "project" plain "** TODO %?"
                                        :if-new (file+head+olp "%<%Y%m%d%H%M%S>-${slug}.org"
                                                                "#+title: ${title}\n#+category: ${title}\n#+filetags: Project"
                                                                ("Tasks"))))))

        (defun my/org-roam-copy-todo-to-today ()
        (interactive)
        (let ((org-refile-keep t) ;; Set this to nil to delete the original!
                (org-roam-dailies-capture-templates
                '(("t" "tasks" entry "%?"
                :if-new (file+head+olp "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n" ("Tasks")))))
                (org-after-refile-insert-hook #'save-buffer)
                today-file
                pos)
        (save-window-excursion
        (org-roam-dailies--capture (current-time) t)
        (setq today-file (buffer-file-name))
        (setq pos (point)))

        ;; Only refile if the target file is different than the current file
        (unless (equal (file-truename today-file)
                        (file-truename (buffer-file-name)))
        (org-refile nil nil (list "Tasks" today-file nil pos)))))

        ;; (add-to-list 'org-after-todo-state-change-hook
        ;;              (lambda ()
        ;;                (when (equal org-state "DONE")
        ;;                  (my/org-roam-copy-todo-to-today))))
)

(setq org-caldav-url "https://mycloud.iancole.me/remote.php/dav/calendars/icole")
(setq org-caldav-calendar-id "personal")
(setq org-caldav-inbox "~/org/calendar.org")
(setq org-icalendar-timezone "America/Los_Angeles")

(use-package wallabag
  :defer t
  :config
  (setq wallabag-host (plist-get (nth 0 (auth-source-search :max 1)) :host)) ;; wallabag server host name
  (setq wallabag-username  (plist-get (nth 0 (auth-source-search :max 1)) :user)) ;; username
  (setq wallabag-password  (funcall (plist-get (nth 0 (auth-source-search :max 1)) :secret))) ;; password
  (setq wallabag-clientid (plist-get (nth 1 (auth-source-search :max 2)) :user)) ;; created with API clients management
  (setq wallabag-secret (funcall (plist-get (nth 1 (auth-source-search :max 2)) :secret))) ;; created with API clients management
  ;; (setq wallabag-db-file "~/OneDrive/Org/wallabag.sqlite") ;; optional, default is saved to ~/.emacs.d/.cache/wallabag.sqlite
  ;; (run-with-timer 0 3540 'wallabag-request-token) ;; optional, auto refresh token, token should refresh every hour
  )
