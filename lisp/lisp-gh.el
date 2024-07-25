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

(with-eval-after-load 'embark
  (keymap-set embark-region-map "G" #'+gh/gist-create))

(defun +gh/pr-create ()
  (interactive)
  (shell-command "gh pr create -w"))

(defvar +gh--search-queries
  '(("Created" . "author:@me")
    ("Assigned" . "assignee:@me")
    ("Mentioned" . "mentions:@me")
    ("Review requests" . "review-requested:@me")))

(defun +gh--pr-list ()
  (if current-prefix-arg
      (let* ((selection (completing-read "Select query: " +gh--search-queries))
             (query (cdr (assoc selection +gh--search-queries))))
        (if query
            (+gh--pr-list-search query)
          (+gh--pr-list-search selection)))
    (+gh--pr-list-default)))

(defun +gh--pr-list-request (command)
  (condition-case nil
      (let ((json (json-read-from-string (shell-command-to-string command))))
        (mapcar (lambda (pr)
                  (let-alist pr
                    (format "#%-10.10s %-80.80s %s:%s" .number .title .author.login .headRefName)))
                json))
    (error nil)))

(defun +gh--pr-list-search (query)
  (let* ((command (format "gh pr list --json number,title,headRefName,author --search \"%s\"" query)))
    (+gh--pr-list-request command)))

(defun +gh--pr-list-default ()
  (let ((command "gh pr list --json number,title,headRefName,author"))
    (+gh--pr-list-request command)))

(defun +gh--pr-number ()
  (let* ((collection (+gh--pr-list))
         (target (completing-read "Select pull request: " collection nil t)))
    (when (string-match "^#\\([0-9]+\\)" target)
      (match-string 1 target))))

(defun +gh/pr-browse (&optional pr-number)
  "Browse a pull request by PR-NUMBER."
  (interactive (list (+gh--pr-number)))
  (shell-command (concat "gh pr view -w " pr-number)))

(defun +gh/pr-checkout (pr-number)
  "Checkout a pull request by PR-NUMBER."
  (interactive (list (+gh--pr-number)))
  (shell-command (concat "gh pr checkout " pr-number)))

(defun +gh/pr-view (pr-number)
  "View a pull request by PR-NUMBER."
  (interactive (list (+gh--pr-number)))
  (when-let* ((output (shell-command-to-string (concat "gh pr view " pr-number)))
              (output-formatted (replace-regexp-in-string "\r" "" output))
              (output-buffer (format "*%s (pull request)*" pr-number)))
    (with-output-to-temp-buffer output-buffer
      (princ output-formatted)
      (with-current-buffer output-buffer
        (gfm-view-mode)
        (keymap-local-set "q" #'quit-window)))))

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
  "y" #'+gh/pr-link)

(with-eval-after-load 'embark
  (add-to-list 'embark-keymap-alist '(github-pull-request . embark-gh-pr-map)))
(with-eval-after-load 'marginalia
  (add-to-list 'marginalia-prompt-categories '("Select pull request" . github-pull-request)))

(bind-key "C-c g v" #'+gh/pr-view)
(bind-key "C-c g c" #'+gh/pr-create)

(provide 'lisp-gh)
