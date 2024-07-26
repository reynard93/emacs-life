(use-package magit
  :pin melpa
  :init
  (setq magit-repository-directories
        '(("~/src" . 1)
          ("~/work" . 1)))
  :config
  (with-eval-after-load 'transient
    ;; git push with skip-ci option
    (transient-append-suffix 'magit-push "-n"
      '("-s" "Skip CI" "--push-option=skip-ci"))

    ;; git push to all remotes
    (defun +magit/push-all (&optional args)
      (interactive (list (magit-push-arguments)))
      (dolist (remote (magit-list-remotes))
        (magit-push-to-remote remote args)))
    (transient-append-suffix 'magit-push "e"
      '("E" "everywhere" +magit/push-all)))

  :custom
  (magit-bury-buffer-function #'quit-window)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :bind (("C-c g b" . magit-checkout)
         ("C-c g B" . magit-blame-addition)
         ("C-c g f" . magit-fetch)
         ("C-c g F" . magit-pull)
         ("C-c g l" . magit-log-current)
         ("C-c g L" . magit-log-buffer-file)))

(provide 'init-git)
