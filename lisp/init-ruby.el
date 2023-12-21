(use-package ruby-ts-mode
  :ensure nil
  :preface
  (defun +eglot/start-ruby-lsp ()
    "Start eglot if a .ruby-lsp directory exists in the project root."
    (interactive)
    (let ((ruby-lsp-path (expand-file-name ".ruby-lsp" (+project/root-dir))))
      (when (file-directory-p ruby-lsp-path)
        (eglot-ensure))))

  :config
  (message "ruby-ts-mode is loaded")
  :hook (ruby-ts-mode . +eglot/start-ruby-lsp))

(use-package bundler
  :pin melpa
  :defer t
  :config
  (message "bundler is loaded"))

(use-package rake
  :pin melpa
  :defer t
  :config
  (message "rake is loaded")
  :custom
  (rake-completion-system 'default))

(use-package rspec-mode
  :pin melpa
  :defer t
  :config
  (message "rspec-mode is loaded"))

(provide 'init-ruby)
