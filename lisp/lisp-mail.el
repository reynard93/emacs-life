(defun +mail/compose (recipient &optional subject)
  "Prepare an email with the given RECIPIENT and SUBJECT."
  (compose-mail recipient subject)
  (message-goto-body))

(defvar +mail--service-alist
  '(("Day One" . "add-to-dayone"))
  "Alist of services with their corresponding auth-source keys.")

(defun +mail/compose-for-service (name)
  "Compose a mail for a specified service."
  (interactive
   (list (completing-read
          "Choose service: " (mapcar #'car +mail--service-alist)
          nil t)))
  (if-let* ((pass-entry (cdr (assoc name +mail--service-alist)))
            (recipient (auth-source-pass-get "email" pass-entry)))
      (+mail/compose recipient)
    (error "Recipient not found for service: %s" name)))

(provide 'lisp-mail)
