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
            (mu4e-refile-folder  . "/Personal/[Gmail]/All Mail")))

         (make-mu4e-context
          :name "Work"
          :match-func
          (lambda (msg)
            (when msg
              (string-prefix-p "/Work" (mu4e-message-field msg :maildir))))
          :vars
          '((user-mail-address . "james.su@managebac.com")
            (user-full-name    . "James Su")
            (mu4e-sent-folder  . "/Work/[Gmail]/Sent Mail")
            (mu4e-drafts-folder  . "/Work/[Gmail]/Drafts")
            (mu4e-trash-folder  . "/Work/[Gmail]/Trash")
            (mu4e-refile-folder  . "/Work/[Gmail]/All Mail")))))

  (setq mu4e-maildir-shortcuts
        '(
          (:maildir "/Personal/Inbox"             :key ?i)
          (:maildir "/Personal/[Gmail]/Sent Mail" :key ?s)
          (:maildir "/Personal/[Gmail]/Drafts"    :key ?d)
          (:maildir "/Personal/[Gmail]/Trash"     :key ?t)
          (:maildir "/Personal/[Gmail]/All Mail"  :key ?a)
          (:maildir "/Work/Inbox"                 :key ?I)
          (:maildir "/Work/[Gmail]/Sent Mail"     :key ?S)
          (:maildir "/Work/[Gmail]/Drafts"        :key ?D)
          (:maildir "/Work/[Gmail]/Trash"         :key ?T)
          (:maildir "/Work/[Gmail]/All Mail"      :key ?A)))

  (setq mu4e-bookmarks
        '((:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key ?u)
          (:name "Today's messages" :query "date:today..now" :key ?t)
          (:name "Last 7 days" :query "date:7d..now" :hide-unread t :key ?w)
          (:name "Sourcehut lists" :query "flag:list AND to:lists.sr.ht" :key ?s)
          (:name "Codeberg lists" :query "flag:list AND from:codeberg.org" :key ?c)
          (:name "GitHub lists" :query "flag:list AND from:github.com" :key ?g)
          (:name "Jira" :query "fariaedu.atlassian.net" :key ?j)))

  :custom
  (mu4e-update-interval (* 15 60))
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-change-filenames-when-moving t))

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
