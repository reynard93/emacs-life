(use-package elixir-ts-mode
  :pin melpa
  :hook (before-save . elixir-format-before-save)
  :bind (:map my-insert-map ("e e" . +elixir/package-config-insert))
  :config
  (defun elixir-format-before-save ()
    (when (derived-mode-p 'elixir-ts-mode)
      (eglot-format-buffer)))

  (defun +elixir/package-config-insert (package)
    "Fetch PACKAGE's mix.exs config and insert at point."
    (interactive "sPackage: ")
    (let* ((command (format "mix hex.info %s | grep 'Config:' | sed 's/Config: //g'" package))
           (result (shell-command-to-string command)))
      (insert result))))

(use-package heex-ts-mode
  :pin melpa
  :defer t)

(use-package exunit
  :pin melpa
  :hook elixir-ts-mode)

(provide 'init-elixir)
