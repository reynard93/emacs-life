(use-package pass
  :pin melpa
  :defer t
  :preface
  (defun +pass/kill-ring-save-otp-uri (issuer secret)
    "Create and copy the OTP URI to kill ring consisting of issuer and secret."
    (interactive (list (read-string "Issuer: ")
                       (read-passwd "Secret: " t)))
    (let* ((secret (replace-regexp-in-string "\\s-" "" secret))
           (otp-uri (format "otpauth://totp/totp-secret?secret=%s&issuer=%s" secret issuer)))
      (kill-new otp-uri)
      (message "OTP URI is created and copied to kill ring")))
  :init
  (auth-source-pass-enable)

  (defvar-keymap embark-password-store-map
    "c" #'password-store-copy
    "C" #'password-store-otp-token-copy
    "f" #'password-store-copy-field
    "i" #'password-store-insert
    "I" #'password-store-generate
    "r" #'password-store-rename
    "e" #'password-store-edit
    "k" #'password-store-remove
    "U" #'password-store-url)

  (with-eval-after-load 'embark
    (add-to-list 'embark-keymap-alist '(password-store . embark-password-store-map)))
  (with-eval-after-load 'marginalia
    (add-to-list 'marginalia-prompt-categories '("Password entry" . password-store)))

  :config
  (message "pass is loaded")
  :bind ("C-c p" . password-store-copy))

(provide 'init-pass)
