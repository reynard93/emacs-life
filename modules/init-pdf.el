(use-package pdf-tools
  :pin nongnu
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-loader-install :no-query))

(use-package saveplace-pdf-view
  :pin melpa
  :after pdf-tools)

(provide 'init-pdf)
