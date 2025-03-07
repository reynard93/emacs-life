(use-package flycheck
  :hook ruby-ts-mode)

(use-package consult-flycheck
  :after (consult flycheck))

(provide 'init-syntax-checker)
