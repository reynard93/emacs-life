(use-package magit
  :pin melpa
  :defer t
  :config
  (message "magit is loaded")
  (add-to-list 'savehist-additional-variables 'log-edit-comment-ring)

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

(use-package forge
  :pin melpa
  :after magit
  :config
  (message "forge is loaded")
  :custom
  (forge-topic-list-limit '(20 . 5)))

(use-package browse-at-remote
  :pin melpa
  :defer t
  :config
  (message "browse-at-remote is loaded")
  :custom
  (browse-at-remote-add-line-number-if-no-region-selected nil))

(use-package git-gutter
  :pin melpa
  :config
  (message "git-gutter is loaded")
  (global-git-gutter-mode 1))

(use-package git-timemachine
  :pin melpa
  :defer t
  :config
  (message "git-timemachine is loaded"))

(provide 'init-git)
