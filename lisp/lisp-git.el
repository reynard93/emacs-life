(defun +git/create-backup-commit ()
  (interactive)
  (when-let ((default-directory (+project/root-dir)))
    (let ((commit-message (format-time-string "Auto-backup on %Y-%m-%d at %H:%M:%S")))
      (shell-command (format "git add --all && git commit -m \"%s\"" commit-message)))))

(defun +gh/gist-create (&optional arg)
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
      (kill-new (string-trim (thing-at-point 'line))))
    (kill-buffer output-buffer)))

(defun +gh/pr-create ()
  (interactive)
  (shell-command "gh pr create -w"))

(defun +gh/pr-list ()
  "List all pull requests for current repository."
  (interactive)
  (if-let ((pr-list (+gh--pr-list)))
      (completing-read "Select pull request: " pr-list nil t)
    (user-error "PR list is empty or not a GitHub repo")))

(defun +gh--pr-list ()
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

(defun +gh--pr-number (target)
  (string-match "^#\\([0-9]+\\)" target)
  (match-string 1 target))

(defun +gh/pr-checkout (target)
  "Checkout a pull request with gh command line."
  (when-let ((pr-number (+gh--pr-number target)))
    (shell-command (concat "gh pr checkout " pr-number))))

(defun +gh/pr-browse (target)
  "Browse a pull request with gh command line."
  (when-let ((pr-number (+gh--pr-number target)))
    (shell-command (concat "gh pr view -w " pr-number))))

(defun +gh/pr-view (target)
  "View a pull request with gh command line."
  (when-let* ((pr-number (+gh--pr-number target))
              (output (shell-command-to-string (concat "gh pr view " pr-number)))
              (formatted-output (replace-regexp-in-string "\r" "" output))
              (buffer-name (format "*gh-pr-view %s*" pr-number)))
    (with-output-to-temp-buffer buffer-name
      (with-current-buffer buffer-name
        (gfm-mode)
        (evil-local-set-key 'normal (kbd "q") 'quit-window))
      (princ formatted-output))))

(defun +gh/pr-link (target)
  "Copy a pull request's URL with gh command line."
  (when-let ((pr-number (+gh--pr-number target)))
    (let* ((command (format "gh pr view %s --json url --template '{{.url}}'" pr-number))
           (output (shell-command-to-string command)))
      (kill-new output)
      (message "Copied URL: %s" output))))

(defvar-keymap embark-gh-pr-map
  "c" #'+gh/pr-checkout
  "o" #'+gh/pr-browse
  "v" #'+gh/pr-view
  "y" #'+gh/pr-link)

(with-eval-after-load 'embark
  (add-to-list 'embark-keymap-alist '(github-pull-request . embark-gh-pr-map)))
(with-eval-after-load 'marginalia
  (add-to-list 'marginalia-prompt-categories '("Select pull request" . github-pull-request)))

(provide 'lisp-git)
