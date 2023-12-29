(use-package eglot
  :ensure nil
  :defer t
  :config
  (message "eglot is loaded")
  (add-to-list 'eglot-server-programs '((elixir-ts-mode heex-ts-mode) . ("nextls" "--stdio=true")))
  (add-to-list 'eglot-server-programs '((ruby-ts-mode) "ruby-lsp")))

(provide 'init-eglot)
