(defun +github/create-pull-request ()
  (interactive)
  (shell-command "gh pr create -w"))

(defun +github/browse-pull-request ()
  (interactive)
  (shell-command "gh pr view -w"))

(defun +github/yank-pull-request ()
  (interactive)
  (let* ((command "gh pr view --json url --template '{{.url}}'")
         (output (shell-command-to-string command)))
    (if (string-prefix-p "https:" output)
        (progn
          (kill-new output)
          (message "Copied URL: %s" output))
      (user-error "PR list is empty or not a GitHub repo"))))

(defun +github/checkout-pull-request ()
  "Select a GitHub pull request to checkout."
  (interactive)
  (if-let* ((pr-list (+github--gh-pr-list '("number" "title")))
            (formatted-pr-list (mapcar (lambda (pr)
                                         (format "%s: %s"
                                                 (alist-get 'number pr)
                                                 (alist-get 'title pr)))
                                       pr-list))
            (selected-pr (completing-read "Select PR: " formatted-pr-list))
            (pr-number (progn (string-match "^\\([0-9]+\\):" selected-pr)
                              (match-string 1 selected-pr))))
      (shell-command (concat "gh pr checkout " pr-number))
    (user-error "PR list is empty or not a GitHub repo")))

(defun +github--gh-pr-list (fields)
  (let ((command (concat "gh pr list --json " (string-join fields ","))))
    (condition-case nil
        (json-read-from-string (shell-command-to-string command))
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
