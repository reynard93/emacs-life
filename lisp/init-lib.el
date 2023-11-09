(defun yejun/find-file-in-project (dir)
  (let ((project (project-current nil dir)))
    (project-find-file-in nil nil project)))

(provide 'init-lib)
