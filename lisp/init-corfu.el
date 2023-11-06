(use-package corfu
  :config
  (message "corfu is loaded")
  (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-scroll-margin 5))

(use-package orderless
  :after vertico
  :config
  (message "orderless is loaded")
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(provide 'init-corfu)
