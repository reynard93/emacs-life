(defun +mail/compose (recipient subject)
  "Prepare an email with the given recipient and subject."
  (compose-mail recipient subject)
  (message-goto-body))

(provide 'lisp-mail)
