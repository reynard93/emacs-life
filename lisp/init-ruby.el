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

(use-package yaml-mode
  :pin nongnu
  :defer t
  :mode "Procfile\\'"
  :mode "Procfile\\.dev\\'"
  :config
  (message "yaml-mode is loaded")
  (evil-collection-init 'yaml-mode))

(provide 'init-ruby)
