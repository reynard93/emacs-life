(use-package elixir-ts-mode
  :pin melpa
  :hook (elixir-ts-mode . elixir-format-before-save)
  :bind (:map my-insert-map ("e e" . my/elixir-package-insert))
  :config
  (defun elixir-format-before-save ()
    (when (derived-mode-p 'elixir-ts-mode)
      (add-hook 'before-save-hook #'eglot-format-buffer nil t)))

  (defun my/elixir-package-insert (package)
    "Insert Hex PACKAGE config at point."
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
