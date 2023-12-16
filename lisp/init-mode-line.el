(use-package doom-modeline
  :pin melpa
  :preface
  (column-number-mode 1)
  (display-time-mode 1)
  :config
  (message "doom-modeline is loaded")
  (doom-modeline-mode 1))

(use-package anzu
  :pin melpa
  :defer 1
  :config
  (message "anzu is loaded")
  (global-anzu-mode 1))

(use-package evil-anzu
  :pin melpa
  :after (evil anzu)
  :config
  (message "evil-anzu is loaded"))

(provide 'init-mode-line)
