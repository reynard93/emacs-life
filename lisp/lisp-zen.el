(define-minor-mode yejun/zen-mode
  "Toggle logos-focus-mode."
  :init-value nil
  :global nil
  (if yejun/zen-mode
      (logos-focus-mode 1)
    (logos-focus-mode -1)))

(provide 'lisp-zen)
