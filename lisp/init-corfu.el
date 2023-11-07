(use-package corfu
  :defer 1
  :config
  (message "corfu is loaded")
  (global-corfu-mode)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-scroll-margin 5))

(use-package tempel
  :defer 1
  :config
  (message "tempel is loaded")
  :custom
  (tempel-trigger-prefix "<")
  :hook
  ((conf-mode prog-mode text-mode) . tempel-setup-capf))

(defun tempel-setup-capf ()
  (interactive)
  (setq-local completion-at-point-functions
              (cons #'tempel-expand
                    completion-at-point-functions)))

(provide 'init-corfu)
