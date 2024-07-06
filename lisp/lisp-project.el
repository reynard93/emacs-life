(defun +project/browse-files (&optional dir)
  (when-let* ((project (project-current nil dir))
              (default-directory (project-root project)))
    (project-find-file-in nil nil project)))

(defun +project/root-dir (&optional dir)
  (let ((project (project-current nil dir)))
    (unless project (user-error "Not in a project"))
    (project-root project)))

(provide 'lisp-project)
