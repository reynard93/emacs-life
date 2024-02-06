(defun +project/browse-files (&optional dir)
  (interactive)
  (when-let* ((project (project-current nil dir))
              (default-directory (project-root project)))
    (project-find-file-in nil nil project)))

(defun +project/root-dir (&optional dir)
  (interactive)
  (let ((project (project-current nil dir)))
    (unless project (user-error "Not in a project"))
    (project-root project)))

(defun +project/search (&optional dir thing)
  (interactive)
  (consult-ripgrep
   (+project/root-dir dir)
   (when thing (thing-at-point thing))))

(defun +project/search-for-symbol-at-point ()
  (interactive)
  (+project/search nil 'symbol))

(provide 'lisp-project)
