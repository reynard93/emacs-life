(use-package magit
  :pin melpa
  :init
  (setq magit-repository-directories
        '(("~/src" . 1)
          ("~/work" . 1)))
  :config
  (message "magit is loaded")
  :custom
  (magit-define-global-key-bindings nil)
  (magit-bury-buffer-function #'quit-window)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

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
  (with-eval-after-load 'evil
    (evil-define-key 'motion 'global
      "]d" #'git-gutter:next-hunk
      "[d" #'git-gutter:previous-hunk))
  :custom
  (git-gutter:added-sign " ")
  (git-gutter:deleted-sign " ")
  (git-gutter:modified-sign " ")
  :hook prog-mode)

(use-package git-timemachine
  :pin melpa
  :defer t
  :config
  (message "git-timemachine is loaded"))

(use-package git-link
  :pin melpa
  :defer t
  :config
  (message "git-link is loaded"))

(provide 'init-git)
