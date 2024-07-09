(use-package pulsar
  :config
  (message "pulsar is loaded")
  (pulsar-global-mode 1))

(use-package goggles
  :pin melpa
  :config
  (message "goggles is loaded")
  (setq-default goggles-pulse t)
  :hook (prog-mode text-mode))

(provide 'init-pulse)
