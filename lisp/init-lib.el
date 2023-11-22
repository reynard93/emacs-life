(defun yejun/find-file-in-project (&optional dir)
  (interactive)
  (when-let* ((project (project-current nil dir))
              (default-directory (project-root project)))
    (project-find-file-in nil nil project)))

(defun +project-root-dir (&optional dir)
  (if-let ((project (project-current nil dir)))
      (project-root project)
    nil))

(defun yejun/search-project (&optional dir thing)
  (interactive)
  (consult-ripgrep
   (+project-root-dir dir)
   (when thing (thing-at-point thing))))

(defun yejun/search-project-for-symbol-at-point ()
  (interactive)
  (yejun/search-project nil 'symbol))

(defun yejun/search-buffer (&optional thing)
  (interactive)
  (consult-line (when thing (thing-at-point thing))))

(defun yejun/search-buffer-for-symbol-at-point ()
  (interactive)
  (yejun/search-buffer 'symbol))

(defun yejun/yank-buffer-path (&optional buffer dir)
  "Save the buffer path into the kill-ring.
If BUFFER is not nil, find filename of BUFFER, otherwise, find
filename of `current-buffer'. If DIR is not nil, get a relative
file path, otherwise, get a full file path with
`abbreviate-file-name'."
  (interactive)
  (if-let* ((filename (if buffer
                          (buffer-filename buffer)
                        (buffer-file-name)))
            (path (if dir
                      (file-relative-name filename dir)
                    (abbreviate-file-name filename))))
      (progn
        (kill-new path)
        (message "Copied path: %s" path))
    (user-error "Buffer is not visiting a file")))

(defun yejun/yank-buffer-path-relative-to-project ()
  "Save the relative buffer path into the kill-ring.
The path is relative to `project-current'."
  (interactive)
  (yejun/yank-buffer-path nil (+project-root-dir)))

(defun yejun/delete-this-file ()
  (interactive)
  (when-let* ((buffer (current-buffer))
              (filename (buffer-file-name buffer))
              (path (abbreviate-file-name filename)))
    (when (y-or-n-p (format "Really delete %s? " path))
      (move-file-to-trash path)
      (kill-buffer buffer)
      (message "Deleted %s" path))))

(defun yejun/reveal-in-finder ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (call-process "open" nil 0 nil "-R" filename)
      (user-error "Buffer is not visiting a file"))))

(provide 'init-lib)
