(defvar-local +zen-toggle nil
  "Non-nil if +zen-toggle is enabled.")

(defun +zen/toggle ()
  (interactive)
  (if +zen-toggle
      (progn
        (logos-focus-mode -1)
        (setq-local +zen-toggle nil))
    (progn
      (logos-focus-mode 1)
      (setq-local +zen-toggle t))))

(provide 'lisp-zen)
