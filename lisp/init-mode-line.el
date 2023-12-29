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
  :config
  (message "anzu is loaded")
  (global-anzu-mode 1))

(provide 'init-mode-line)
