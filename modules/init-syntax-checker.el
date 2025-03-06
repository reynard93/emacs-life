(use-package flycheck
  :pin nongnu
  :hook ruby-ts-mode)

(use-package consult-flycheck
  :pin nongnu
  :after (consult flycheck))

(provide 'init-syntax-checker)
