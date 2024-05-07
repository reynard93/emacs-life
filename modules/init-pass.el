(use-package pass
  :pin melpa
  :defer t
  :init
  (auth-source-pass-enable)
  :config
  (message "pass is loaded")
  :bind ( :prefix-map password-store-prefix-map
          :prefix "C-c p"
          ("a" . password-store-otp-append)
          ("A" . password-store-otp-append-from-image)
          ("e" . password-store-edit)
          ("i" . password-store-insert)
          ("I" . password-store-otp-insert)
          ("K" . password-store-remove)
          ("p" . password-store-copy)
          ("r" . password-store-rename)
          ("t" . password-store-otp-token-copy)))

(provide 'init-pass)
