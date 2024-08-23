(use-package alfred-browser
  :ensure nil
  :load-path "site-lisp/"
  :if (eq system-type 'darwin)
  :bind
  (("C-c i b m" . alfred-browser-link-in-markdown-format)
   ("C-c i b o" . alfred-browser-link-in-org-format)
   ("C-c i b t" . alfred-browser-link-title)
   ("C-c i b u" . alfred-browser-link-url)))

(use-package browser-hist
  :pin melpa
  :bind (:map search-map ("U" . browser-hist-search)))

(provide 'init-browser)
