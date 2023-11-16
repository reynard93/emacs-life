(use-package elixir-mode
  :pin melpa
  :config
  (message "elixir-mode is loaded")

  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 `((elixir-mode elixir-ts-mode heex-ts-mode) .
                   ("nextls" "--stdio=true"))))

  (defun elixir-format-before-save ()
    (when (derived-mode-p 'elixir-mode)
      (eglot-format-buffer)))

  :hook
  ((elixir-mode elixir-ts-mode heex-ts-mode) . eglot-ensure)
  (before-save . elixir-format-before-save))

(provide 'init-elixir)
