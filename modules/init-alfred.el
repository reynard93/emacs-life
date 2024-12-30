(use-package alfred-browser
  :ensure nil
  :load-path "site-lisp/"
  :if (eq system-type 'darwin)
  :bind
  (:map my-insert-map
        ("b m" . alfred-browser-md-link)
        ("b o" . alfred-browser-org-link)
        ("b t" . alfred-browser-title)
        ("b u" . alfred-browser-url)))

(provide 'init-alfred)
