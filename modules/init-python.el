(use-package python
  :elpaca nil
  :hook ((python-mode python-ts-mode) . python-format-before-save)
  :config
  (defun python-format-before-save ()
    (add-hook 'before-save-hook #'eglot-format-buffer nil t)))

(provide 'init-python)
