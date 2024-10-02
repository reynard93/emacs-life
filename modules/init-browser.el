(use-package alfred-browser
  :ensure nil
  :load-path "site-lisp/"
  :if (eq system-type 'darwin)
  :bind
  ( :map my-insert-map
    ("b m" . alfred-browser-link-in-markdown-format)
    ("b o" . alfred-browser-link-in-org-format)
    ("b t" . alfred-browser-link-title)
    ("b u" . alfred-browser-link-url)))

(use-package browser-hist
  :pin melpa
  :bind (:map search-map ("U" . browser-hist-search)))

(provide 'init-browser)
