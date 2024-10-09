(use-package eglot
  :ensure nil
  :hook ((ruby-ts-mode nix-mode elixir-ts-mode heex-ts-mode c-ts-mode) . eglot-ensure)
  :custom
  (eglot-ignored-server-capabilities '(:completionProvider))
  :config
  (dolist (mode '((ruby-ts-mode . ("ruby-lsp"))
                  (nix-mode . ("nixd"))
                  ((elixir-ts-mode heex-ts-mode) . ("nextls" "--stdio=true"))
                  (c-ts-mode . ("clangd"
                                ;; feature options
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                ;; misc options
                                "-j=8"
                                "--pch-storage=memory"
                                ;; logging options
                                "--log=error"
                                ))))
    (add-to-list 'eglot-server-programs mode)))

(provide 'init-eglot)
