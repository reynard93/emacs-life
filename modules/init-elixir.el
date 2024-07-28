(use-package heex-ts-mode
  :pin melpa
  :defer t)

(use-package elixir-ts-mode
  :pin melpa
  :defer t
  :config
  (defun elixir-format-before-save ()
    (when (derived-mode-p 'elixir-ts-mode)
      (eglot-format-buffer)))
  :hook (before-save . elixir-format-before-save))

(provide 'init-elixir)
