(defun +github/create-pull-request ()
  (interactive)
  (shell-command "gh pr create -w"))

(defun +github/browse-pull-request ()
  (interactive)
  (shell-command "gh pr view -w"))

(defun +github/list-pull-requests ()
  "List all pull requests for current repository."
  (interactive)
  (if-let ((pr-list (+github--gh-pr-list)))
      (completing-read "Select pull request: " pr-list nil t)
    (user-error "PR list is empty or not a GitHub repo")))

(defun +github--gh-pr-list ()
  (let ((command (concat "gh pr list --json number,title,headRefName,author")))
    (condition-case nil
        (let ((json (json-read-from-string (shell-command-to-string command))))
          (mapcar (lambda (pr)
                    (let* ((number (alist-get 'number pr))
                           (title (alist-get 'title pr))
                           (branch (alist-get 'headRefName pr))
                           (author (alist-get 'author pr))
                           (login (alist-get 'login author)))
                      (format "#%-10.10s %-80.80s %s:%s" number title login branch)))
                  json))
      (error nil))))

(defun +github/create-gist-region-or-buffer (&optional arg)
  (interactive "P")
  (let ((filename (buffer-name))
        (output-buffer "*gist-output*")
        (public (if arg " --public" "")))
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

(defun +sourcehut/create-paste-region-or-buffer (&optional arg)
  (interactive "P")
  (let ((filename (read-string "Enter filename: " (buffer-name)))
        (output-buffer "*paste-output*")
        (public (if arg " --visibility public" "")))
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
  (when-let ((default-directory (+project/root-dir)))
    (let ((commit-message (format-time-string "Auto-backup on %Y-%m-%d at %H:%M:%S")))
      (shell-command (format "git add --all && git commit -m \"%s\"" commit-message)))))

(provide 'lisp-git)
