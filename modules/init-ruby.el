(use-package ruby-ts-mode
  :ensure nil
  :defer t
  :config
  (message "ruby-ts-mode is loaded"))

(use-package inf-ruby
  :defer t
  :config
  (message "inf-ruby is loaded")
  (defun +inf-ruby/console-rails-in-root-dir ()
    (interactive)
    (when-let ((dir (+project/root-dir)))
      (inf-ruby-console-rails dir)))

  :custom
  (inf-ruby-console-environment "development"))

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
