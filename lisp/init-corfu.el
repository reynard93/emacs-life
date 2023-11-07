(use-package corfu
  :config
  (message "corfu is loaded")
  (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-scroll-margin 5))

(provide 'init-corfu)
