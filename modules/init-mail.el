(setq user-full-name "Yejun Su"
      user-mail-address "goofan.su@gmail.com")

(use-package mu4e
  :ensure nil
  :defer t
  :init
  (setq mu4e-maildir "~/.mail")

  :config
  (message "mu4e is loaded")

  (setq mu4e-maildir-shortcuts
        '((:maildir "/Inbox"             :key ?i)
          (:maildir "/[Gmail]/Sent Mail" :key ?s)
          (:maildir "/[Gmail]/Drafts"    :key ?d)
          (:maildir "/[Gmail]/Trash"     :key ?t)
          (:maildir "/[Gmail]/All Mail"  :key ?a)))

  (setq mu4e-bookmarks
        '((:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key ?u)
          (:name "Today's messages" :query "date:today..now" :key ?t)
          (:name "Last 7 days" :query "date:7d..now" :hide-unread t :key ?w)
          (:name "Messages with images" :query "mime:image/*" :key ?p)
          (:name "SourceHut lists" :query "lists.sr.ht" :key ?s)
          (:name "GitHub notifications" :query "github.com" :key ?g)))

  :custom
  (mu4e-update-interval (* 15 60))
  (mu4e-get-mail-command "mbsync -a"))

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
