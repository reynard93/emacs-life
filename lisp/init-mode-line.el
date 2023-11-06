(column-number-mode 1)
(display-time-mode 1)

(use-package doom-modeline
  :pin melpa
  :init
  (message "doom-modeline is loaded")
  (doom-modeline-mode 1))

(provide 'init-mode-line)
