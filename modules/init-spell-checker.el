(use-package jinx
  :ensure nil
  :hook text-mode
  :bind ([remap ispell-word] . jinx-correct))

(provide 'init-spell-checker)
