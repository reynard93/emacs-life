(use-package osx-dictionary
  :pin melpa
  :config
  (message "osx-dictionary is loaded")
  :bind ( :map search-map
          ("t" . osx-dictionary-search-word-at-point)))

(use-package dash-at-point
  :pin melpa
  :config
  (message "dash-at-point is loaded")
  :bind ( :map search-map
          ("k" . dash-at-point)
          ("K" . dash-at-point-with-docset)))

(use-package browser-hist
  :pin melpa
  :config
  (message "browser-hist is loaded")
  (setf (alist-get 'firefox browser-hist-db-paths)
        "$HOME/Library/Application Support/Firefox/Profiles/*/places.sqlite")
  :custom
  (browser-hist-cache-timeout (* 24 60 60))
  (browser-hist-default-browser 'firefox)
  :bind ( :map search-map
          ("u" . browser-hist-search)))

(use-package deadgrep
  :pin melpa
  :config
  (message "deadgrep is loaded")
  (evil-set-initial-state 'deadgrep-mode 'emacs)
  :bind ( :map search-map
          ("d". deadgrep)))

(provide 'init-lookup)
