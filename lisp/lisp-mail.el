(defun +mail/compose (recipient &optional subject)
  "Prepare an email with the given RECIPIENT and SUBJECT."
  (compose-mail recipient subject)
  (message-goto-body))

(provide 'lisp-mail)
