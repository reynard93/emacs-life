(use-package python
  :ensure nil
  :hook ((python-mode python-ts-mode) . python-format-before-save)
  :config
  (defun python-format-before-save ()
    (when (derived-mode-p 'python-base-mode)
      (add-hook 'before-save-hook #'eglot-format-buffer nil t))))

(provide 'init-python)
