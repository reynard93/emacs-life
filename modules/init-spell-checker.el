(use-package flyspell-correct
  :pin melpa
  :after flyspell
  :bind ( :map flyspell-mode-map
          ("C-M-i" . flyspell-correct-wrapper)))

(provide 'init-spell-checker)
