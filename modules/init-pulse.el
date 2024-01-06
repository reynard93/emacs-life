(use-package pulsar
  :config
  (message "pulsar is loaded")
  (pulsar-global-mode 1)
  :custom
  (pulsar-face 'pulsar-magenta)
  (pulsar-pulse-functions
   '(ace-window
     delete-window
     delete-other-windows
     evil-goto-line
     evil-goto-first-line
     evil-goto-last-change
     evil-scroll-page-down
     evil-scroll-page-up
     evil-scroll-line-to-bottom
     evil-scroll-line-to-center
     evil-scroll-line-to-top
     evil-window-prev
     evil-window-next
     evil-window-left
     evil-window-right
     evil-window-up
     evil-window-down
     evil-window-split
     evil-window-vsplit
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
