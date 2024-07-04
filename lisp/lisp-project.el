(defun +project/browse-files (&optional dir)
  (when-let* ((project (project-current nil dir))
              (default-directory (project-root project)))
    (project-find-file-in nil nil project)))

(defun +project/root-dir (&optional dir)
  (let ((project (project-current nil dir)))
    (unless project (user-error "Not in a project"))
    (project-root project)))

(defun +project/search ()
  (interactive)
  (let ((dir (+project/root-dir)))
    (funcall-interactively #'consult-ripgrep dir)))

(defun +project/search-for-symbol-at-point ()
  (interactive)
  (let ((dir (+project/root-dir))
        (initial (thing-at-point 'symbol t)))
    (funcall-interactively #'consult-ripgrep dir initial)))

(defun yejun/browse-emacs-config ()
  (interactive)
  (+project/browse-files user-emacs-directory))

(defun yejun/browse-nix-config ()
  (interactive)
  (+project/browse-files "~/.config/nix-config/"))

(defun yejun/browse-blog ()
  (interactive)
  (+project/browse-files "~/src/yejun.dev/"))

(defun yejun/browse-org-directory ()
  (interactive)
  (+project/browse-files org-directory))

(provide 'lisp-project)
