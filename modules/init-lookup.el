(use-package osx-dictionary
  :pin melpa
  :defer t
  :config
  (message "osx-dictionary is loaded")
  (evil-set-initial-state 'osx-dictionary-mode 'emacs))

(use-package dash-at-point
  :pin melpa
  :defer t
  :config
  (message "dash-at-point is loaded"))

(use-package deadgrep
  :defer t
  :pin melpa
  :config
  (message "deadgrep is loaded")
  (evil-set-initial-state 'deadgrep-mode 'emacs))

(provide 'init-lookup)
