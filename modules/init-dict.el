(use-package osx-dictionary
  :pin melpa
  :if (eq system-type 'darwin)
  :bind
  ( :map search-map
    ("t" . osx-dictionary-search-word-at-point)
    ("T" . osx-dictionary-search-input)))

(use-package jinx
  :ensure nil
  :hook text-mode
  :bind (([remap ispell-word] . jinx-correct)
         ("<f12>" . jinx-mode)))

(provide 'init-dict)
