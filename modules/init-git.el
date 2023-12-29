(use-package magit
  :pin melpa
  :config
  (message "magit is loaded")
  :custom
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package transient
  :ensure nil
  :after magit
  :config
  (message "transient is loaded")

  ;; git push with skip-ci option
  (transient-append-suffix 'magit-push "-n"
    '("-s" "Skip CI" "--push-option=skip-ci"))

  ;; git push to all remotes
  (defun magit-push-all (&optional args)
    (interactive (list (magit-push-arguments)))
    (dolist (remote (magit-list-remotes))
      (magit-push-to-remote remote args)))
  (transient-append-suffix 'magit-push "e"
    '("E" "everywhere" magit-push-all)))

(use-package browse-at-remote
  :pin melpa
  :defer t
  :config
  (message "browse-at-remote is loaded")
  :custom
  (browse-at-remote-add-line-number-if-no-region-selected nil))

(use-package git-gutter
  :pin melpa
  :after evil
  :config
  (message "git-gutter is loaded")
  (global-git-gutter-mode 1)
  (evil-define-key 'motion 'global
    "]d" #'git-gutter:next-hunk
    "[d" #'git-gutter:previous-hunk)
  :custom
  (git-gutter:added-sign " ")
  (git-gutter:deleted-sign " ")
  (git-gutter:modified-sign " "))

(use-package git-timemachine
  :pin melpa
  :defer t
  :config
  (message "git-timemachine is loaded"))

(provide 'init-git)
