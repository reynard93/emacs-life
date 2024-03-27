(defvar-local +org-preview-toggle nil
  "Non-nil if +org-preview-toggle is enabled.")

(defun +org/run-hook ()
  (setq-local evil-auto-indent nil))

(defun +org/preview-toggle ()
  (interactive)
  (if +org-preview-toggle
      (progn
        (olivetti-mode -1)
        (org-indent-mode -1)
        (org-superstar-mode -1)
        (setq-local org-hide-emphasis-markers nil)
        (setq-local +org-preview-toggle nil))
    (progn
      (olivetti-mode 1)
      (org-indent-mode 1)
      (org-superstar-mode 1)
      (setq-local org-hide-emphasis-markers t)
      (setq-local +org-preview-toggle t))))

(provide 'lisp-org)
