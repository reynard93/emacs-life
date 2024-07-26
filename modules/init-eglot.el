(use-package eglot
  :ensure nil
  :defer t
  :init
  (defun +eglot/start-ruby-lsp ()
    "Start eglot if a .ruby-lsp directory exists in the project root."
    (interactive)
    (let ((ruby-lsp-path (expand-file-name ".ruby-lsp" (+project/root-dir))))
      (when (file-directory-p ruby-lsp-path)
        (setq-local eglot-ignored-server-capabilities '(:hoverProvider :completionProvider))
        (eglot-ensure))))

  :config
  (add-to-list 'eglot-server-programs '((elixir-ts-mode heex-ts-mode) . ("nextls" "--stdio=true")))
  (add-to-list 'eglot-server-programs '((ruby-ts-mode) "ruby-lsp"))
  :hook
  (ruby-ts-mode . +eglot/start-ruby-lsp)
  (elixir-ts-mode . eglot-ensure)
  (heex-ts-mode . eglot-ensure))

(provide 'init-eglot)
