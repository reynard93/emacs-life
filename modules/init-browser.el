(use-package alfred-browser
  :ensure nil
  :load-path "site-lisp/"
  :if (eq system-type 'darwin)
  :bind
  ( :map my-insert-map
    ("b m" . alfred-browser-md-link)
    ("b o" . alfred-browser-org-link)
    ("b t" . alfred-browser-title)
    ("b u" . alfred-browser-url)))

(use-package browser-hist
  :pin melpa
  :bind (:map search-map ("U" . browser-hist-search)))

(provide 'init-browser)
