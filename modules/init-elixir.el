(use-package heex-ts-mode
  :pin melpa
  :defer t)

(use-package elixir-ts-mode
  :pin melpa
  :init
  (defun elixir-format-before-save ()
    (when (derived-mode-p 'elixir-ts-mode)
      (apheleia-format-buffer 'mix-format)))
  :hook (before-save . elixir-format-before-save))

(provide 'init-elixir)
