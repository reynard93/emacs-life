(use-package eglot
  :ensure nil
  :hook
  (ruby-ts-mode . eglot-ensure)
  (nix-mode . eglot-ensure)
  :config
  (dolist (mode '((ruby-ts-mode . ("ruby-lsp"))
                  (nix-mode . ("nixd"))))
    (add-to-list 'eglot-server-programs mode)))

(provide 'init-eglot)
