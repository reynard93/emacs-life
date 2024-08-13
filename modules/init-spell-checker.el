(use-package jinx
  :ensure nil
  :hook text-mode
  :bind
  (([remap ispell-word] . jinx-correct)
   ("<f12>" . jinx-mode))
  :custom
  (jinx-languages "en_US"))

(provide 'init-spell-checker)
