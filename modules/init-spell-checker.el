(use-package flyspell-correct
  :pin melpa
  :after flyspell
  :config
  (message "flyspell-correct is loaded")
  :bind ( :map flyspell-mode-map
          ("C-M-i" . flyspell-correct-wrapper))
  :hook
  (git-commit-setup . git-commit-turn-on-flyspell))

(provide 'init-spell-checker)
