(use-package doc-view
  :ensure nil
  :custom
  (doc-view-resolution 300)
  (doc-view-mupdf-use-svg t))

(use-package saveplace-pdf-view
  :pin melpa
  :after doc-view)

(provide 'init-pdf)
