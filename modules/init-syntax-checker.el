(use-package flycheck
  :pin nongnu
  :config
  ;; (setq flycheck-ruby-rubocop-executable "bundle exec rubocop")
  :hook ruby-ts-mode)

(use-package consult-flycheck
  :pin nongnu
  :after (consult flycheck))

(provide 'init-syntax-checker)
