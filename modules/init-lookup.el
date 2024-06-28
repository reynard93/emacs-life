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
  :config
  (message "browser-hist is loaded")
  (setf (alist-get 'firefox browser-hist-db-paths)
        "$HOME/Library/Application Support/Firefox/Profiles/*/places.sqlite")
  :custom
  (browser-hist-cache-timeout (* 24 60 60))
  (browser-hist-default-browser 'firefox)
  :bind ("M-s u" . browser-hist-search))

(provide 'init-lookup)
