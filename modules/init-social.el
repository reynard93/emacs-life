;;; -*- lexical-binding: t -*-

(use-package alert
  :custom
  (alert-default-style 'notifier))

(use-package circe
  :defer t)

(use-package oauth2
  :defer t)

(use-package slack
  :ensure (:host github :repo "emacs-slack/emacs-slack")
  :commands (slack-start slack-stop slack-select-rooms slack-select-unread-rooms)
  :bind (("C-c S s" . slack-start)
         ("C-c S k" . slack-stop)
         ("C-c S r" . slack-select-rooms)
         ("C-c S u" . slack-select-unread-rooms))
  :custom
  (slack-log-level 'info)
  (slack-buffer-emojify t)
  :config
  (defcustom my/slack-teams nil
    "Slack team names to register from pass.

For a team named \"myteam\", create pass entries:
- slack/myteam/token
- slack/myteam/cookie   (optional; required for some xoxc tokens)"
    :type '(repeat string))

  ;; Default to your workspace; override by customizing `my/slack-teams`.
  (unless my/slack-teams
    (setq my/slack-teams '("grain-team")))

  (defun my/slack-register-team-from-pass (team)
    "Register TEAM using secrets from pass.

Reads secrets from:
- slack/TEAM/token
- slack/TEAM/cookie (optional)

If `pass`/gpg is not working, falls back to prompting."
    (let* ((token (condition-case nil
                      (auth-source-pass-get 'secret (format "slack/%s/token" team))
                    (error nil)))
           (cookie (condition-case nil
                       (auth-source-pass-get 'secret (format "slack/%s/cookie" team))
                     (error nil))))
      (unless token
        (setq token (read-string (format "Slack token for %s: " team))))
      (slack-register-team
       :name team
       :token token
       :cookie cookie
       :full-and-display-names t
       :default t)))

  (dolist (team my/slack-teams)
    (condition-case err
        (my/slack-register-team-from-pass team)
      (error (message "Slack not configured for %s yet: %s" team (error-message-string err))))))

;; Telegram client (telega.el)
(use-package telega
  :ensure (:host github :repo "zevlg/telega.el")
  :commands (telega)
  :bind ("C-c t" . telega)
  :custom
  (telega-server-libs-prefix
   (or (getenv "TDLIB_PREFIX")
       "/usr/local"))
  (telega-server-command
   (or (executable-find "telega-server")
       (expand-file-name "elpaca/repos/telega/server/telega-server" user-emacs-directory)
       "telega-server"))
  :config
  ;; Optional: read Telegram API credentials from pass
  ;; pass entries:
  ;; - telegram/app-id
  ;; - telegram/app-hash
  (let ((app-id (auth-source-pass-get 'secret "telegram/app-id"))
        (app-hash (auth-source-pass-get 'secret "telegram/app-hash")))
    (when (and app-id app-hash)
      (setq telega-app-id (string-to-number app-id)
            telega-app-hash app-hash)))

  ;; Desktop notifications via `alert` (you already set alert-default-style)
  (telega-notifications-mode 1)
  (telega-mode-line-mode 1))

(provide 'init-social)
