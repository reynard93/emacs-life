(use-package jinx
  :ensure nil
  :hook text-mode
  :bind (([remap ispell-word] . jinx-correct)
         ("<f12>" . jinx-mode)))

(provide 'init-spell-checker)
