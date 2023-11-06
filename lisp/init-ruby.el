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
  (message "rspec-mode is loaded")
  :hook (ruby-mode . yejun/maybe-enable-rspec-mode))

(defun yejun/maybe-enable-rspec-mode ()
  (when-let* ((project (project-current))
              (root (project-root project))
              (rspec-config (expand-file-name ".rspec" root)))
    (when (file-exists-p rspec-config)
      (rspec-mode 1))))

(provide 'init-ruby)
