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
;;(setq doom-font (font-spec :family "CaskaydiaCove Nerd Font" :size 14)
;;      doom-big-font (font-spec :family "CaskaydiaCove Nerd Font" :size 24)
;;      doom-variable-pitch-font (font-spec :family "JetBrainsMono Nerd Font Mono" :size 14))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

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

(bind-key "C-c C-y" 'org-todo-yesterday)

(cl-loop for file in '("/usr/local/bin/zsh" "/usr/bin/zsh")
         when (file-exists-p file)
         do (progn
              (setq shell-file-name file)
              (cl-return)))
(setenv "SHELL" shell-file-name)

(setq global-auto-revert-mode t)

(setq-default tab-width 2)
(setq json-reformat:indent-width 2)
(setq mocha-snippets-use-fat-arrows t)
(setq mocha-snippets-add-space-after-function-keyword t)

;; use eslint from node modules dicetory
(add-hook 'prog-mode-hook 'flycheck-mode)
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(use-package js2-mode
  :mode "\\.js\\'"
  :config
  (customize-set-variable 'js2-include-node-externs t))

(use-package web-mode
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(setq auth-sources
      '((:source "~/.authinfo.gpg")))

(add-hook 'sh-mode-hook 'shfmt-on-save-mode)

(after! org
  (map! (:leader
         (:prefix "m"
                  (:prefix "c"
                   :desc "Start Pomodoro" "p" #'org-pomodoro
                   ))))
  ;; (setq org-capture-templates
  ;;       `(("h" "Habit"
  ;;          entry (file "~/org/roam/20220222120745-habits.org")
  ;;          "* TODO %? :Habit:\nSCHEDULED: <%<%Y-%m-%d %a .+1d>>\n:PROPERTIES:\n:CREATED: %U\n:STYLE: habit\n:REPEAT_TO_STATE: TODO\n:LOGGING: DONE(!)\n:ARCHIVE: %%s_archive::* Habits\n:END:\n%U\n"
  ;;          :kill-buffer t)
  ;;         ))
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

  (add-hook 'after-init-hook 'mu4e-alert-disable-notifications)

  (add-to-list 'mu4e-bookmarks
               '( :name  "Unread Today"
                  :query "flag:unread AND NOT flag:trashed AND date:today..now AND maildir:/Personal/INBOX"
                  :key   ?o))
  (add-to-list 'mu4e-bookmarks
               '( :name  "Unread Inbox"
                  :query "flag:unread AND NOT flag:trashed AND maildir:/Personal/INBOX"
                  :key   ?i))

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

(after! gnutls
  (add-to-list 'gnutls-trustfiles
               (expand-file-name "~/.cert/protonmail.smtp.crt"))
  )

;; https://github.com/zerolfx/copilot.el/issues/40
;; accept completion from copilot and fallback to company
(defun my-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (indent-for-tab-command)))

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map company-active-map
         ("<tab>" . 'my-tab)
         ("TAB" . 'my-tab)
         :map company-mode-map
         ("<tab>" . 'my-tab)
         ("TAB" . 'my-tab)))

(after! copilot
  (setq copilot-node-executable "~/.asdf/shims/node")
  )

(add-to-list 'exec-path (expand-file-name "~/go/bin"))

;; ;;;###autoload
;; (define-derived-mode gts-mode typescript-mode "GTS"
;;   "Major mode for editing Glimmer TypeScript files."
;;   (when (treesit-available-p)
;;     (condition-case nil
;;         (progn
;;           (treesit-parser-create 'glimmer)
;;           (treesit-parser-create 'glimmer-typescript)
;;           (treesit-major-mode-setup))
;;       (error (message "Failed to initialize glimmer-ts support - falling back to basic mode"))))

;;   ;; Ensure typescript decorators are highlighted
;;   (font-lock-add-keywords nil '(("@\\([A-Za-z]+\\)" . font-lock-type-face))))

;; (add-to-list 'auto-mode-alist '("\\.gts\\'" . gts-mode))
;;(define-derived-mode hbs-mode web-mode "Handlebars mode" "Major mode for handlebars")
;;(add-to-list 'auto-mode-alist '("\\.hbs\\'" . hbs-mode))

;; LSP configuration
(after! lsp-mode
  (add-to-list 'lsp-language-id-configuration
               '(gts-mode . "typescript"))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "glint-language-server")
    :major-modes '(gts-mode)

    :server-id 'glint))

  (add-to-list 'lsp-language-id-configuration
               '(hbs-mode . "hbs"))
  (lsp-register-client
   ;; Git clone language server from https://github.com/lifeart/ember-language-server/tree/component-context-info-origin
   ;; And build it
   (make-lsp-client :new-connection (lsp-stdio-connection (list "node" "/Users/icole/Workspace/ember-language-server/bin/ember-language-server.js" "--stdio"))
                    :major-modes '(hbs-mode gts-mode)
                    :priority -1  ; Lower priority than Glint
                    :add-on? t
                    :server-id 'ember-ls)))

;; Enable LSP for GTS files
;; (add-hook! 'gts-mode-hook #'lsp!)

(use-package polymode
  :commands (polymode poly-hostmode poly-innermode)
  :config
  ;; 1) Define a host mode for TypeScript
  (define-hostmode pm-gts-typescript-hostmode
    :mode 'typescript-mode)

  ;; 2) Define an innermode for the <template> block
  (define-innermode pm-gts-template-innermode
    :mode 'handlebars-mode              ;; or 'handlebars-mode if you prefer
    :head-matcher "<template>"
    :tail-matcher "</template>"
    :head-mode 'body
    :tail-mode 'body)

  ;; 3) Tie them together in a single polymode
  (define-polymode pm-polymode-gts
    :hostmode 'pm-gts-typescript-hostmode
    :innermodes '(pm-gts-template-innermode))

  ;; 4) Associate .gts files with this polymode
  (add-to-list 'auto-mode-alist '("\\.gts\\'" . pm-polymode-gts)))
