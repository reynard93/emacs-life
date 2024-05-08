(defun +embark--gh-parse-pr-number (target)
  (progn
    (string-match "^#\\([0-9]+\\)" target)
    (match-string 1 target)))

(defun +embark/gh-pr-checkout (target)
  "Checkout a pull request with gh command line."
  (when-let ((pr-number (+embark--gh-parse-pr-number target)))
    (shell-command (concat "gh pr checkout " pr-number))))

(defun +embark/gh-pr-browse (target)
  "Browse a pull request with gh command line."
  (when-let ((pr-number (+embark--gh-parse-pr-number target)))
    (shell-command (concat "gh pr view -w " pr-number))))

(defun +embark/gh-pr-copy-url (target)
  "Copy a pull request's URL with gh command line."
  (when-let ((pr-number (+embark--gh-parse-pr-number target)))
    (let* ((command (format "gh pr view %s --json url --template '{{.url}}'" pr-number))
           (output (shell-command-to-string command)))
      (kill-new output)
      (message "Copied URL: %s" output))))

(defvar-keymap embark-gh-pr-map
  "b" #'+embark/gh-pr-browse
  "c" #'+embark/gh-pr-checkout
  "U" #'+embark/gh-pr-copy-url)

(with-eval-after-load 'embark
  (add-to-list 'embark-keymap-alist '(github-pull-request . embark-gh-pr-map)))
(with-eval-after-load 'marginalia
  (add-to-list 'marginalia-prompt-categories '("Select pull request" . github-pull-request)))

;; Open any buffer by splitting any window
;; https://karthinks.com/software/fifteen-ways-to-use-embark/#open-any-buffer-by-splitting-any-window
(eval-when-compile
  (defmacro +embark--aw-action (fn)
    `(defun ,(intern (concat "+embark/aw-" (symbol-name fn))) ()
       ,(format "Open %s buffer selected with ace-window." (symbol-name fn))
       (interactive)
       (with-demoted-errors "%s"
         (require 'ace-window)
         (let ((aw-dispatch-always t))
           (aw-switch-to-window (aw-select nil))
           (call-interactively (symbol-function ',fn)))))))

(with-eval-after-load 'embark
  (keymap-set embark-file-map     "o" (+embark--aw-action find-file))
  (keymap-set embark-buffer-map   "o" (+embark--aw-action switch-to-buffer))
  (keymap-set embark-bookmark-map "o" (+embark--aw-action bookmark-jump)))

(provide 'lisp-embark)
