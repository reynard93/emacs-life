(setq user-full-name "Yejun Su"
      user-mail-address "goofan.su@gmail.com")

(use-package mu4e
  :ensure nil
  :defer t
  :init
  (setq mu4e-maildir "~/.mail")

  :config
  (message "mu4e is loaded")
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-context-policy 'pick-first)

  (setq mu4e-contexts
        (list
         (make-mu4e-context
          :name "Personal"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/Personal" (mu4e-message-field msg :maildir))))
          :vars
          '((user-mail-address . "goofan.su@gmail.com")
            (user-full-name    . "Yejun Su")
            (mu4e-sent-folder  . "/Personal/[Gmail]/Sent Mail")
            (mu4e-drafts-folder  . "/Personal/[Gmail]/Drafts")
            (mu4e-trash-folder  . "/Personal/[Gmail]/Trash")
            (mu4e-refile-folder  . "/Personal/[Gmail]/All Mail")))))

  (setq mu4e-maildir-shortcuts
        '((:maildir "/Personal/Inbox"             :key ?i :hide t)
          (:maildir "/Personal/[Gmail]/Sent Mail" :key ?s)
          (:maildir "/Personal/[Gmail]/Drafts"    :key ?d)
          (:maildir "/Personal/[Gmail]/Trash"     :key ?t :hide t)
          (:maildir "/Personal/[Gmail]/All Mail"  :key ?a :hide t)))

  (setq mu4e-bookmarks
        '((:name "Inbox"       :key ?i :query "m:/Personal/Inbox" :hide t)
          (:name "Unread"      :key ?u :query "flag:unread AND NOT flag:trashed")
          (:name "Today"       :key ?t :query "date:today..now")
          (:name "Last 7 days" :key ?w :query "date:7d..now" :hide-unread t)
          (:name "Sourcehut"   :key ?s :query "flag:list AND to:lists.sr.ht" :hide t)
          (:name "Codeberg"    :key ?c :query "flag:list AND from:codeberg.org" :hide t)
          (:name "GitHub"      :key ?g :query "flag:list AND from:github.com" :hide t)))

  :custom
  (mu4e-update-interval (* 15 60))
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-change-filenames-when-moving t)
  (mu4e-read-option-use-builtin nil)
  (mu4e-completing-read-function #'completing-read))

(use-package message
  :ensure nil
  :config
  (message "message is loaded")
  :custom
  (message-sendmail-f-is-evil t)
  (message-sendmail-extra-arguments '("--read-envelope-from"))
  (message-send-mail-function #'message-send-mail-with-sendmail))

(use-package sendmail
  :ensure nil
  :config
  (message "sendmail is loaded")
  :custom
  (sendmail-program (executable-find "msmtp"))
  (send-mail-function #'smtpmail-send-it))

(provide 'init-mail)
