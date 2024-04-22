(use-package heex-ts-mode
  :pin melpa
  :defer t
  :config
  (message "heex-ts-mode is loaded"))

(use-package elixir-ts-mode
  :pin melpa
  :defer t
  :config
  (message "elixir-ts-mode is loaded")
  (defun elixir-format-before-save ()
    (when (derived-mode-p 'elixir-ts-mode)
      (eglot-format-buffer)))
  :hook (before-save . elixir-format-before-save))

(provide 'init-elixir)
