(use-package pass
  :pin melpa
  :defer t
  :config
  (message "pass is loaded")
  :bind (("C-c p p" . password-store-copy)
         ("C-c p P" . password-store-otp-token-copy)
         ("C-c p i" . password-store-insert)
         ("C-c p I" . password-store-otp-insert)
         ("C-c p e" . password-store-edit)
         ("C-c p r" . password-store-rename)
         ("C-c p R" . password-store-remove)
         ("C-c p a" . password-store-otp-append)
         ("C-c p A" . password-store-otp-append-from-image)
         ("C-c p u" . yejun/otp-key-uri)))

(defun yejun/otp-key-uri (issuer secret)
  "Create and copy the OTP key URI consisting of issuer and secret."
  (interactive (list (read-string "Issuer: ")
                     (read-passwd "Secret: " t)))
  (let* ((secret (replace-regexp-in-string "\\s-" "" secret))
         (otp-uri (format "otpauth://totp/totp-secret?secret=%s&issuer=%s" secret issuer)))
    (kill-new otp-uri)
    (message "OTP key URI created and copied.")))

(provide 'init-pass)
