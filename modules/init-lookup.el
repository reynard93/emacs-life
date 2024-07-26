(use-package osx-dictionary
  :pin melpa
  :bind ( :map search-map
          ("t" . osx-dictionary-search-word-at-point)
          ("T" . osx-dictionary-search-input)))

(use-package dash-at-point
  :pin melpa
  :bind ( :map search-map
          ("k" . dash-at-point)
          ("K" . dash-at-point-with-docset)))

(use-package browser-hist
  :pin melpa
  :config
  (setf (alist-get 'firefox browser-hist-db-paths)
        "$HOME/Library/Application Support/Firefox/Profiles/*/places.sqlite")
  :custom
  (browser-hist-cache-timeout (* 24 60 60))
  (browser-hist-default-browser 'firefox)
  :bind ( :map search-map
          ("U" . browser-hist-search)))

(use-package deadgrep
  :pin melpa
  :bind ( :map search-map
          ("D". deadgrep)))

(provide 'init-lookup)
