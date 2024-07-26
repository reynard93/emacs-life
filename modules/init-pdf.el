(use-package pdf-tools
  :pin melpa
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-loader-install :no-query))

(provide 'init-pdf)
