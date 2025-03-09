(use-package transient
  :ensure)

;; if performance issues
;; refer to https://jakemccrary.com/blog/2020/11/14/speeding-up-magit/
(use-package magit
  :ensure
  :after transient
  :defer 1
  :bind
  (("C-c g b" . magit-checkout)
   ("C-c g B" . magit-blame-addition)
   ("C-c g f" . magit-fetch)
   ("C-c g F" . magit-pull)
   ("C-c g l" . magit-log-current)
   ("C-c g L" . magit-log-buffer-file))

  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  :config
  (with-eval-after-load 'transient
    ;; git push with skip-ci option
    (transient-append-suffix 'magit-push "-n"
      '("-s" "Skip CI" "--push-option=skip-ci"))

    ;; git push to all remotes
    (defun my/magit-push-all (&optional args)
      (interactive (list (magit-push-arguments)))
      (dolist (remote (magit-list-remotes))
        (magit-push-to-remote remote args)))
    (transient-append-suffix 'magit-push "e"
      '("E" "everywhere" my/magit-push-all))))

(use-package browse-at-remote
  :bind
  (("C-c g o" . browse-at-remote)
   ("C-c g y" . browse-at-remote-kill)))

;; sometimes this can go crazy, showing up everywhere
(use-package sideline-blame
  :init
  (setq sideline-backends-left '((sideline-blame . down)))
  :config
  (global-sideline-mode))

(provide 'init-git)
