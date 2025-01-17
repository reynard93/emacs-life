(use-package ruff-format
  :pin melpa
  :hook ((python-mode python-ts-mode) . ruff-format-on-save-mode))

(provide 'init-python)
