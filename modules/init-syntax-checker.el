(use-package flycheck
  ;; :bind
  ;; ("C-c f" . flycheck-show-buffer-diagnostics) ;; dn think i using this keymap, show diagnostic also wh fn?
  :hook ((prog-mode ruby-ts-mode)
         . flycheck-mode))

(use-package flycheck-jest
  :after flycheck
  )

(use-package consult-flycheck
  :after (consult flycheck))

(provide 'init-syntax-checker)
