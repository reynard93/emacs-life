(use-package vertico
  :init
  (message "vertico is loaded")
  (vertico-mode)
  :custom
  (vertico-cycle t)
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)))

(provide 'init-minibuffer)
