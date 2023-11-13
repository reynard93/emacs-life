(defun yejun/find-file-in-project (dir)
  (let ((project (project-current nil dir)))
    (project-find-file-in nil nil project)))

(defun yejun/search-in-project (dir &optional thing-p)
  (let ((thing (when thing-p (thing-at-point thing-p))))
    (consult-ripgrep dir thing)))

(defun yejun/current-project-root ()
  (or (when-let ((project (project-current)))
        (project-root project))
      (user-error "Not in a project")))

(provide 'init-lib)
