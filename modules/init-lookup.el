(use-package osx-dictionary
  :pin melpa
  :config
  (message "osx-dictionary is loaded")
  :bind ("M-s t" . osx-dictionary-search-word-at-point))

(use-package dash-at-point
  :pin melpa
  :config
  (message "dash-at-point is loaded")
  :bind (("M-s k" . dash-at-point)
         ("M-s K" . dash-at-point-with-docset)))

(use-package browser-hist
  :pin melpa
  :defer t
  :config
  (message "browser-hist is loaded")
  :custom
  (browser-hist-default-browser 'firefox))

(provide 'init-lookup)
