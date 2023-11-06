(use-package vertico
  :init
  (message "vertico is loaded")
  (vertico-mode)
  :custom
  (vertico-cycle t)
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :config
  (message "vertico-directory is loaded")
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :after vertico
  :config
  (message "marginalia is loaded")
  (marginalia-mode))

(use-package which-key
  :defer 1
  :config
  (message "which-key is loaded")
  (which-key-mode 1)
  :custom
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-min-display-lines 6)
  (which-key-min-display-columns nil)
  (which-key-add-column-padding 1))

(provide 'init-minibuffer)
