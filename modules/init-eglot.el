(use-package eglot
  :ensure nil
  :defer t
  :init
  (defun +eglot/start-ruby-lsp ()
    "Start eglot if a .ruby-lsp directory exists in the project root."
    (interactive)
    (let ((ruby-lsp-path
           (condition-case nil
               (expand-file-name ".ruby-lsp" (project-root (project-current)))
             (error nil))))
      (when (and ruby-lsp-path (file-directory-p ruby-lsp-path))
        (setq-local eglot-ignored-server-capabilities '(:hoverProvider :completionProvider))
        (eglot-ensure))))
  :config
  (dolist (mode '(((elixir-ts-mode heex-ts-mode) . ("nextls" "--stdio=true"))
                  (ruby-ts-mode . ("ruby-lsp"))
                  (nix-mode . ("nixd"))))
    (add-to-list 'eglot-server-programs mode))
  :hook
  ((elixir-ts-mode heex-ts-mode) . eglot-ensure)
  (ruby-ts-mode . +eglot/start-ruby-lsp)
  (nix-mode . eglot-ensure))

(provide 'init-eglot)
