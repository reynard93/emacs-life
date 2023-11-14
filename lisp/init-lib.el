(defun yejun/find-file-in-project (&optional dir)
  (interactive)
  (let ((project (project-current nil dir)))
    (project-find-file-in nil nil project)))

(defun yejun/search-project (&optional dir thing)
  (interactive)
  (let* ((project (project-current nil dir))
         (root-dir (project-root project)))
    (consult-ripgrep root-dir (when thing (thing-at-point thing)))))

(defun yejun/search-project-for-symbol-at-point ()
  (interactive)
  (yejun/search-project nil 'symbol))

(defun yejun/search-buffer (&optional thing)
  (interactive)
  (consult-line (when thing (thing-at-point thing))))

(defun yejun/search-buffer-for-symbol-at-point ()
  (interactive)
  (yejun/search-buffer 'symbol))

(provide 'init-lib)
