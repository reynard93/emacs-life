(use-package osx-dictionary
  :pin melpa
  :config
  (message "osx-dictionary is loaded")
  (evil-set-initial-state 'osx-dictionary-mode 'emacs)
  :bind ("M-s t" . osx-dictionary-search-word-at-point))

(use-package dash-at-point
  :pin melpa
  :config
  (message "dash-at-point is loaded")
  :bind (("M-s k" . dash-at-point)
         ("M-s K" . dash-at-point-with-docset)))

(use-package deadgrep
  :pin melpa
  :config
  (message "deadgrep is loaded")
  (evil-set-initial-state 'deadgrep-mode 'emacs)
  :bind ("M-s d" . deadgrep))

(provide 'init-lookup)
