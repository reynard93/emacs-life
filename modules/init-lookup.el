(use-package osx-dictionary
  :pin melpa
  :defer t
  :config
  (message "osx-dictionary is loaded")
  :bind ( :map search-map
          ("t" . osx-dictionary-search-word-at-point)))

(use-package browser-hist
  :pin melpa
  :defer t
  :config
  (message "browser-hist is loaded")
  (setf (alist-get 'firefox browser-hist-db-paths)
        "$HOME/Library/Application Support/Firefox/Profiles/*/places.sqlite")
  :custom
  (browser-hist-cache-timeout (* 24 60 60))
  (browser-hist-default-browser 'firefox)
  :bind ( :map search-map
          ("u" . browser-hist-search)))

(provide 'init-lookup)
