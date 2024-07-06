(use-package magit
  :pin melpa
  :demand t
  :init
  (setq magit-repository-directories
        '(("~/src" . 1)
          ("~/work" . 1)))
  :config
  (message "magit is loaded")
  :custom
  (magit-bury-buffer-function #'quit-window)
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :bind (("C-c g b" . magit-checkout)
         ("C-c g B" . magit-blame-addition)
         ("C-c g f" . magit-fetch)
         ("C-c g F" . magit-pull)
         ("C-c g l" . magit-log-current)
         ("C-c g L" . magit-log-buffer-file)))

(use-package browse-at-remote
  :pin melpa
  :config
  (message "browse-at-remote is loaded")
  :custom
  (browse-at-remote-add-line-number-if-no-region-selected nil)
  :bind ("C-c g o" . browse-at-remote))

(use-package git-gutter
  :pin melpa
  :config
  (message "git-gutter is loaded")
  :custom
  (git-gutter:added-sign " ")
  (git-gutter:deleted-sign " ")
  (git-gutter:modified-sign " ")
  :hook (prog-mode text-mode))

(use-package git-timemachine
  :pin melpa
  :defer t
  :config
  (message "git-timemachine is loaded")
  :bind ("C-c g t" . git-timemachine-toggle))

(use-package git-link
  :pin melpa
  :config
  (message "git-link is loaded")
  :bind ("C-c g y" . git-link))

(provide 'init-git)
