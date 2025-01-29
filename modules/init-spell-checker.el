(use-package jinx
  :ensure nil
  :hook org-mode
  :bind
  (([remap ispell-word] . jinx-correct)
   ("C-c J" . jinx-languages)
   ("<f12>" . jinx-mode))
  :custom
  (jinx-languages "en_US en_GB"))

(provide 'init-spell-checker)
