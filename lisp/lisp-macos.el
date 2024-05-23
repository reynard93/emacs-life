(defun +macos/reveal-in-finder ()
  (interactive)
  (if-let ((filename (buffer-file-name)))
      (start-process "finder" nil "open" "-R" filename)
    (user-error "Buffer is not visiting any file")))

(defun +macos/reveal-project-in-finder ()
  (interactive)
  (when-let* ((project-root (+project/root-dir))
              (filename (expand-file-name project-root)))
    (start-process "finder" nil "open" "-R" filename)))

(defun +macos/notify (title body &optional sound-name)
  (let* ((sound (or sound-name "Default"))
         (script (format "display notification \"%s\" with title \"%s\" sound name \"%s\""
                         body title sound)))
    (do-applescript script)))

(provide 'lisp-macos)
