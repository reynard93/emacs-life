(use-package nov
  :pin melpa
  :defer t
  :mode ("\\.epub\\'" . nov-mode))

(use-package calibre
  :defer t
  :custom
  (calibre-libraries '(("main" . "~/Calibre Library/"))))

(provide 'init-epub)
