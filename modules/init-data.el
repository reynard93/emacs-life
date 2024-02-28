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

(use-package sqlite-mode-extras
  :ensure nil
  :load-path "vendor/sqlite"
  :after sqlite-mode
  :config
  (message "sqlite-mode-extras is loaded")
  :bind ( :map sqlite-mode-map
          ("n" . next-line)
          ("p" . previous-line)
          ("b" . sqlite-mode-extras-backtab-dwim)
          ("f" . sqlite-mode-extras-tab-dwim)
          ("+" . sqlite-mode-extras-add-row)
          ("D" . sqlite-mode-extras-delete-row-dwim)
          ("C" . sqlite-mode-extras-compose-and-execute)
          ("E" . sqlite-mode-extras-execute)
          ("S" . sqlite-mode-extras-execute-and-display-select-query)
          ("DEL" . sqlite-mode-extras-delete-row-dwim)
          ("g" . sqlite-mode-extras-refresh)
          ("<backtab>" . sqlite-mode-extras-backtab-dwim)
          ("<tab>" . sqlite-mode-extras-tab-dwim)
          ("RET" . sqlite-mode-extras-ret-dwim)))

(provide 'init-data)
