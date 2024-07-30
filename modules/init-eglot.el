(use-package eglot
  :ensure nil
  :hook ((ruby-ts-mode nix-mode) . eglot-ensure)
  :custom
  (eglot-ignored-server-capabilities '(:completionProvider))
  :config
  (dolist (mode '((ruby-ts-mode . ("ruby-lsp"))
                  (nix-mode . ("nixd"))))
    (add-to-list 'eglot-server-programs mode)))

(provide 'init-eglot)
