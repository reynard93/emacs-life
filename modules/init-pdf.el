(use-package pdf-tools
  :pin melpa
  :magic ("%PDF" . pdf-view-mode)
  :config
  (message "pdf-tools is loaded")
  (pdf-loader-install :no-query))

(provide 'init-pdf)
