(use-package pulsar
  :config
  (message "pulsar is loaded")
  (pulsar-global-mode 1)
  (add-hook 'fontaine-set-preset-hook #'pulsar-pulse-line)
  :custom
  (pulsar-face 'pulsar-magenta)
  (pulsar-pulse-functions
   '(ace-window
     delete-window
     delete-other-windows
     tab-new
     tab-close
     tab-next
     tab-previous
     widen)))

(use-package goggles
  :pin melpa
  :init
  (setq-default goggles-pulse t)
  :config
  (message "goggles is loaded")
  :hook (prog-mode text-mode))

(provide 'init-pulse)
