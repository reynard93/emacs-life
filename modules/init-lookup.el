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

(provide 'init-lookup)
