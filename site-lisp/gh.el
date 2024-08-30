;;; gh.el -- Code for GitHub CLI -*- lexical-binding: t -*-

;;; Commentary:
;;
;; https://cli.github.com/manual/

;;; Code:

(defun gh-gist-create ()
  (interactive)
  (let ((filename (buffer-name))
        (output-buffer "*gist-output*"))
    (shell-command-on-region
     (if (use-region-p) (region-beginning) (point-min))
     (if (use-region-p) (region-end) (point-max))
     (format "gh gist create --filename %s -" filename)
     output-buffer)
    (with-current-buffer output-buffer
      (goto-char (point-max))
      (forward-line -1)
      (kill-new (thing-at-point 'url)))
    (kill-buffer output-buffer)))

(defun gh-pr-create ()
  (interactive)
  (shell-command "gh pr create -w"))

(defvar gh--pr-list-command
  "gh pr list --json number,title,headRefName,author")

(defvar gh--pr-search-queries
  '("author:@me"
    "assignee:@me"
    "mentions:@me"
    "review-requested:@me"
    "head:release"
    "head:hotfix"
    "is:merged"))

(defun gh--pr-list ()
  (if current-prefix-arg
      (let* ((query (completing-read-multiple "Select query: " gh--pr-search-queries))
             (query-string (string-join query " "))
             (command (format "%s --search \"%s\"" gh--pr-list-command query-string)))
        (gh--pr-list-request command))
    (gh--pr-list-request gh--pr-list-command)))

(defun gh--pr-list-request (command)
  (condition-case nil
      (let ((json (json-read-from-string (shell-command-to-string command))))
        (mapcar (lambda (pr)
                  (let-alist pr
                    (format "#%-10.10s %-80.80s %s:%s" .number .title .author.login .headRefName)))
                json))
    (error nil)))

(defun gh--pr-number ()
  (let* ((collection (gh--pr-list))
         (target (completing-read "Select pull request: " collection nil t)))
    (when (string-match "^#\\([0-9]+\\)" target)
      (match-string 1 target))))

(defun gh-pr-browse (&optional pr-number)
  "Browse a pull request by PR-NUMBER."
  (interactive (list (gh--pr-number)))
  (shell-command (concat "gh pr view -w " pr-number)))

(defun gh-pr-checkout (pr-number)
  "Checkout a pull request by PR-NUMBER."
  (interactive (list (gh--pr-number)))
  (shell-command (concat "gh pr checkout " pr-number)))

(defun gh-pr-view (pr-number)
  "View a pull request by PR-NUMBER."
  (interactive (list (gh--pr-number)))
  (when-let* ((output (shell-command-to-string (concat "gh pr view " pr-number)))
              (output-formatted (replace-regexp-in-string "\r" "" output))
              (output-buffer (format "*%s (pull request)*" pr-number)))
    (with-output-to-temp-buffer output-buffer
      (princ output-formatted)
      (with-current-buffer output-buffer
        (gfm-view-mode)
        (keymap-local-set "q" #'quit-window)))))

(defun gh-pr-link (pr-number)
  "Copy a pull request's URL by PR-NUMBER."
  (interactive (list (gh--pr-number)))
  (let* ((command (format "gh pr view %s --json url --template '{{.url}}'" pr-number))
         (output (shell-command-to-string command)))
    (kill-new output)
    (message "Copied URL: %s" output)))

(provide 'gh)
