(define-minor-mode header-line-mode
  "Toggle header-line display in the current buffer."
  :init-value nil
  :global nil
  (if header-line-mode
      (setq-local header-line-format
                  '(:eval
                    (if buffer-file-name
                        (abbreviate-file-name buffer-file-name)
                      (buffer-name))))
    (setq-local header-line-format nil)))

(provide 'header-line)
