(use-package pdf-tools
  :if (display-graphic-p)
  :pin melpa
  :config
  (message "pdf-tools is loaded")
  (pdf-loader-install))

(provide 'init-pdf)
