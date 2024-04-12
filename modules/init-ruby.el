(use-package ruby-ts-mode
  :ensure nil
  :config
  (message "ruby-ts-mode is loaded"))

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
