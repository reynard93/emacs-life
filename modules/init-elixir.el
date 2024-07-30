(use-package heex-ts-mode
  :pin melpa
  :defer t)

(use-package elixir-ts-mode
  :pin melpa
  :hook (before-save . elixir-format-before-save)
  :config
  (message "load")
  (defun elixir-format-before-save ()
    (when (derived-mode-p 'elixir-ts-mode)
      (apheleia-format-buffer 'mix-format))))

(provide 'init-elixir)
