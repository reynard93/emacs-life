(use-package eglot
  :ensure nil
  :config
  (dolist (mode '((ruby-ts-mode . ("ruby-lsp"))
                  (nix-mode . ("nixd"))))
    (add-to-list 'eglot-server-programs mode))
  :hook
  (ruby-ts-mode . eglot-ensure)
  (nix-mode . eglot-ensure))

(provide 'init-eglot)
