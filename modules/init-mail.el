;;; -*- lexical-binding: t -*-

(setq user-full-name "Reynard"
      user-mail-address "reynard@grain.com.sg")

(use-package message
  :ensure nil
  :custom
  (message-sendmail-f-is-evil t)
  (message-sendmail-extra-arguments '("--read-envelope-from"))
  (message-send-mail-function #'message-send-mail-with-sendmail))

(use-package sendmail
  :ensure nil
  :custom
  (sendmail-program (executable-find "msmtp"))
  (send-mail-function #'smtpmail-send-it))

(use-package notmuch
  
  :hook (notmuch-mua-send . notmuch-mua-attachment-check)
  :bind
  (("C-c m" . notmuch)
   ("C-x m" . notmuch-mua-new-mail))
  :custom
  ;; General UI
  (notmuch-show-logo nil)
  (notmuch-column-control 1.0)
  (notmuch-hello-auto-refresh t)
  (notmuch-hello-recent-searches-max 20)
  (notmuch-hello-thousands-separator "")
  (notmuch-show-all-tags-list t)
  (notmuch-hello-sections
   '(notmuch-hello-insert-saved-searches
     notmuch-hello-insert-recent-searches
     notmuch-hello-insert-tags-section))
  ;; Search
  (notmuch-search-oldest-first nil)
  (notmuch-search-result-format
   '(("date" . "%12s  ")
     ("count" . "%-7s  ")
     ("authors" . "%-20.20s  ")
     ("subject" . "%-80.80s  ")
     ("tags" . "(%s)")))
  (notmuch-tree-result-format
   '(("date" . "%12s  ")
     ("authors" . "%-20.20s  ")
     ((("tree" . "%s")
       ("subject" . "%s"))
      . " %-80.80s  ")
     ("tags" . "(%s)")))
  (notmuch-saved-searches
   `(( :name "inbox"
       :query "tag:inbox"
       :sort-order newest-first
       :key ,(kbd "i"))
     ( :name "unread (inbox)"
       :query "tag:unread and tag:inbox"
       :sort-order newest-first
       :key ,(kbd "u"))
     ))
  ;; Compose
  (notmuch-always-prompt-for-sender nil)
  (notmuch-fcc-dirs "sent +sent -unread")
  ;; Reading
  (notmuch-show-indent-messages-width 0)
  (notmuch-show-part-button-default-action 'notmuch-show-view-part)
  (notmuch-wash-wrap-lines-length 120)
  (notmuch-unthreaded-show-out nil))

(provide 'init-mail)
