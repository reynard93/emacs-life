(defun +embark--gh-parse-pr-number (target)
  (progn
    (string-match "^#\\([0-9]+\\)" target)
    (match-string 1 target)))

(defun +embark/gh-checkout-pull-request (target)
  "Checkout a pull request with gh command line."
  (when-let ((pr-number (+embark--gh-parse-pr-number target)))
    (shell-command (concat "gh pr checkout " pr-number))))

(defun +embark/gh-browse-pull-request (target)
  "Browse a pull request with gh command line."
  (when-let ((pr-number (+embark--gh-parse-pr-number target)))
    (shell-command (concat "gh pr view -w " pr-number))))

(defun +embark/gh-yank-pull-request (target)
  "Copy a pull request's URL with gh command line."
  (when-let ((pr-number (+embark--gh-parse-pr-number target)))
    (let* ((command (format "gh pr view %s --json url --template '{{.url}}'" pr-number))
           (output (shell-command-to-string command)))
      (kill-new output)
      (message "Copied URL: %s" output))))

(with-eval-after-load 'embark
  (keymap-set embark-general-map "g c" #'+embark/gh-checkout-pull-request)
  (keymap-set embark-general-map "g b" #'+embark/gh-browse-pull-request)
  (keymap-set embark-general-map "g y" #'+embark/gh-yank-pull-request))

(provide 'lisp-embark)
