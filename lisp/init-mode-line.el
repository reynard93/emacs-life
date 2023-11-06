(column-number-mode 1)
(display-time-mode 1)

(use-package doom-modeline
  :pin melpa
  :init
  (message "doom-modeline is loaded")
  (doom-modeline-mode 1))

(use-package anzu
  :pin nongnu
  :defer 1
  :config
  (message "anzu is loaded")
  (global-anzu-mode))

(use-package evil-anzu
  :pin nongnu
  :after (evil anzu)
  :config
  (message "evil-anzu is loaded"))

(provide 'init-mode-line)
