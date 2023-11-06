(use-package magit
  :pin nongnu
  :config
  (message "magit is loaded")

  ;; git push with skip-ci option
  (transient-append-suffix 'magit-push "-n"
    '("-s" "Skip CI" "--push-option=skip-ci"))

  ;; git push to all remotes
  (transient-append-suffix 'magit-push "e"
    '("E" "everywhere" magit-push-all))
  (defun magit-push-all (&optional args)
    (interactive (list (magit-push-arguments)))
    (dolist (remote (magit-list-remotes))
      (magit-push-to-remote remote args)))

  :custom
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))

(provide 'init-git)
