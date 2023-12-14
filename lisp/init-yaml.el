(use-package yaml-mode
  :pin nongnu
  :defer t
  :mode "Procfile\\'"
  :mode "Procfile\\.dev\\'"
  :config
  (message "yaml-mode is loaded"))

(provide 'init-yaml)
