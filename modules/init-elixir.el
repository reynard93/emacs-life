(use-package heex-ts-mode
  :pin melpa
  :defer t)

(use-package elixir-ts-mode
  :pin melpa
  :init
  (defun +elixir/package-config (package)
    "Fetch PACKAGE's mix.exs config and insert at point."
    (interactive "sPackage: ")
    (let* ((command (format "mix hex.info %s | grep 'Config:' | sed 's/Config: //g'" package))
           (result (shell-command-to-string command)))
      (insert result)))
  :hook (before-save . elixir-format-before-save)
  :bind ("C-c i e e" . +elixir/package-config)
  :config
  (defun elixir-format-before-save ()
    (when (derived-mode-p 'elixir-ts-mode)
      (eglot-format-buffer))))

(use-package exunit
  :pin melpa
  :hook elixir-ts-mode)

(provide 'init-elixir)
