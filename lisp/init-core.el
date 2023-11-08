(defun yejun/browse-project (dir)
  (let ((project (project-current nil dir)))
    (project-find-file-in nil nil project)))

(provide 'init-core)
