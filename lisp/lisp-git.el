(defun +git/create-backup-commit ()
  (interactive)
  (when-let ((default-directory (+project/root-dir)))
    (let ((commit-message (format-time-string "Auto-backup on %Y-%m-%d at %H:%M:%S")))
      (shell-command (format "git add --all && git commit -m \"%s\"" commit-message)))))

(defun +hut/paste-create (&optional arg)
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
      (kill-new (thing-at-point 'url)))
    (kill-buffer output-buffer)))

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
      (kill-new (thing-at-point 'url)))
    (kill-buffer output-buffer)))

(defun +gh/pr-create ()
  (interactive)
  (shell-command "gh pr create -w"))

(defun +gh--pr-list ()
  (let* ((initial-command "gh pr list --json number,title,headRefName,author")
         (command (if current-prefix-arg
                      initial-command
                    (concat initial-command " --author \"@me\""))))
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

(defun +gh--pr-number ()
  (let* ((collection (+gh--pr-list))
         (history (list (magit-get-current-branch)))
         (target (completing-read "Select pull request: " collection nil t nil 'history)))
    (when (string-match "^#\\([0-9]+\\)" target)
      (match-string 1 target))))

(defun +gh/pr-browse (&optional pr-number)
  "Browse a pull request by PR-NUMBER."
  (interactive (list (+gh--pr-number)))
  (shell-command (concat "gh pr view -w " pr-number)))

(defun +gh/pr-browse-at-remote ()
  "Browse pull request for current branch at remote."
  (interactive)
  (+gh/pr-browse))

(defun +gh/pr-checkout (pr-number)
  "Checkout a pull request by PR-NUMBER."
  (interactive (list (+gh--pr-number)))
  (shell-command (concat "gh pr checkout " pr-number)))

(defun +gh/pr-view (pr-number)
  "View a pull request by PR-NUMBER."
  (interactive (list (+gh--pr-number)))
  (when-let* ((output (shell-command-to-string (concat "gh pr view " pr-number)))
              (formatted-output (replace-regexp-in-string "\r" "" output))
              (buffer-name (format "*gh-pr-view %s*" pr-number)))
    (with-output-to-temp-buffer buffer-name
      (with-current-buffer buffer-name
        (gfm-mode)
        (evil-local-set-key 'normal (kbd "q") 'quit-window))
      (princ formatted-output))))

(defun +gh/pr-link (pr-number)
  "Copy a pull request's URL by PR-NUMBER."
  (interactive (list (+gh--pr-number)))
  (let* ((command (format "gh pr view %s --json url --template '{{.url}}'" pr-number))
         (output (shell-command-to-string command)))
    (kill-new output)
    (message "Copied URL: %s" output)))

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
