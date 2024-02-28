(use-package csv-mode
  :defer t
  :config
  (message "csv-mode is loaded"))

(use-package json-mode
  :defer t
  :config
  (message "json-mode is loaded"))

(use-package sqlite-mode
  :defer t
  :config
  (message "sqlite-mode is loaded")
  (evil-set-initial-state 'sqlite-mode 'emacs))

(provide 'init-data)
