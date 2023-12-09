(defun +project/browse-files (&optional dir)
  (when-let* ((project (project-current nil dir))
              (default-directory (project-root project)))
    (project-find-file-in nil nil project)))

(defun +project/root-dir (&optional dir)
  (if-let ((project (project-current nil dir)))
      (project-root project)
    nil))

(defun +project/search (&optional dir thing)
  (interactive)
  (consult-ripgrep
   (+project/root-dir dir)
   (when thing (thing-at-point thing))))

(defun +project/search-for-symbol-at-point ()
  (interactive)
  (+project/search nil 'symbol))

(defun +buffer/search (&optional thing)
  (interactive)
  (consult-line (when thing (thing-at-point thing))))

(defun +buffer/search-for-symbol-at-point ()
  (interactive)
  (+buffer/search 'symbol))

(defun +buffer/yank-path (&optional buffer dir)
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

(defun +buffer/yank-path-relative-to-project ()
  "Save the relative buffer path into the kill-ring.
The path is relative to `project-current'."
  (interactive)
  (+buffer/yank-path nil (+project/root-dir)))

(defun +file/delete-this-file ()
  (interactive)
  (when-let* ((buffer (current-buffer))
              (filename (buffer-file-name buffer))
              (path (abbreviate-file-name filename)))
    (when (y-or-n-p (format "Really delete %s? " path))
      (move-file-to-trash path)
      (kill-buffer buffer)
      (message "Deleted %s" path))))

(defun +macos/reveal-in-finder ()
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (call-process "open" nil 0 nil "-R" filename)
      (user-error "Buffer is not visiting a file"))))

(defun +github/create-pull-request ()
  (interactive)
  (shell-command "gh pr create -w"))

(defun +github/browse-pull-request ()
  (interactive)
  (shell-command "gh pr view -w"))

(defun +github/create-gist-region-or-buffer (&optional p)
  (interactive "P")
  (let ((filename (buffer-name))
        (output-buffer " *gist-output*")
        (public (if p " --public" "")))
    (shell-command-on-region
     (if (use-region-p) (region-beginning) (point-min))
     (if (use-region-p) (region-end) (point-max))
     (concat "gh gist create --filename " filename public " -")
     output-buffer)
    (with-current-buffer output-buffer
      (goto-char (point-max))
      (forward-line -1)
      (kill-new (thing-at-point 'line)))
    (kill-buffer output-buffer)))

(defun +sourcehut/create-paste-region-or-buffer (&optional p)
  (interactive "P")
  (let ((filename (read-string "Enter filename: " (buffer-name)))
        (output-buffer " *paste-output*")
        (public (if p " --visibility public" "")))
    (shell-command-on-region
     (if (use-region-p) (region-beginning) (point-min))
     (if (use-region-p) (region-end) (point-max))
     (concat "hut paste create --name \"" filename "\"" public)
     output-buffer)
    (with-current-buffer output-buffer
      (goto-char (point-max))
      (forward-line -1)
      (kill-new (thing-at-point 'line)))
    (kill-buffer output-buffer)))

(defun +git/create-backup-commit ()
  (interactive)
  (let ((default-directory (+project/root-dir)))
    (when default-directory
      (let ((commit-message (format-time-string "Auto-backup on %Y-%m-%d at %H:%M:%S")))
        (shell-command (format "git add --all && git commit -m \"%s\"" commit-message))))))

(provide 'init-lib)
