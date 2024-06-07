(setq user-full-name "Yejun Su"
      user-mail-address "goofan.su@gmail.com")

(use-package message
  :ensure nil
  :defer t
  :config
  (message "message is loaded")
  :custom
  (message-sendmail-f-is-evil t)
  (message-sendmail-extra-arguments '("--read-envelope-from"))
  (message-send-mail-function #'message-send-mail-with-sendmail))

(use-package sendmail
  :ensure nil
  :after message
  :config
  (message "sendmail is loaded")
  :custom
  (sendmail-program (executable-find "msmtp"))
  (send-mail-function #'smtpmail-send-it))

(use-package notmuch
  :pin melpa
  :init
  (defun +notmuch/update-mail-and-index ()
    "Update mail and index using mbsync and notmuch."
    (interactive)
    (unless (get-process "notmuch")
      (message "[notmuch] Updating mail and index")
      (let ((notmuch-process (start-process-shell-command "notmuch" "*notmuch-output*" "notmuch new")))
        (set-process-sentinel
         notmuch-process
         (lambda (_process _event)
           (message "[notmuch] Updating mail and index...done"))))))
  :hook
  (notmuch-hello-mode . +notmuch/update-mail-and-index)
  (notmuch-mua-send . notmuch-mua-attachment-check)
  :config
  (message "notmuch is loaded")
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
     notmuch-hello-insert-recent-searches))
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
     ( :name "personal"
       :query "to:goofan.su@gmail.com"
       :sort-order newest-first
       :key ,(kbd "p"))
     ( :name "work"
       :query "to:james.su@managebac.com"
       :sort-order newest-first
       :key ,(kbd "w"))))
  ;; Compose
  (notmuch-always-prompt-for-sender t)
  ;; Reading
  (notmuch-show-indent-messages-width 0)
  (notmuch-show-part-button-default-action 'notmuch-show-view-part)
  (notmuch-wash-wrap-lines-length 120)
  (notmuch-unthreaded-show-out nil)
  :bind (("C-c m" . notmuch)
         ("C-x m" . notmuch-mua-new-mail)))

(use-package notmuch-indicator
  :init
  (setq notmuch-indicator-notmuch-config-file
        (expand-file-name "notmuch/default/config" yejun-config-directory))
  :config
  (message "notmuch-indicator is loaded")
  (notmuch-indicator-mode 1)
  :custom
  (notmuch-indicator-args
   '((:terms "tag:unread and tag:inbox" :label "[A] " :face prot-modeline-indicator-green)))
  (notmuch-indicator-add-to-mode-line-misc-info nil)
  (notmuch-indicator-refresh-count (* 60 3))
  (notmuch-indicator-hide-empty-counters nil)
  (notmuch-indicator-force-refresh-commands '(notmuch-refresh-this-buffer)))

(use-package ol-notmuch
  :pin melpa
  :after notmuch
  :config
  (message "ol-notmuch is loaded"))

(provide 'init-mail)
