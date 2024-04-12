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

(use-package browser-hist
  :vc (browser-hist :url "https://github.com/agzam/browser-hist.el.git")
  :defer t
  :config
  (message "browser-hist is loaded")
  :custom
  (browser-hist-default-browser 'firefox))

(provide 'init-lookup)
