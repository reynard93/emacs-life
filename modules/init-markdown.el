(use-package markdown-mode
  :pin nongnu
  :defer t
  :config
  (message "markdown-mode is loaded")
  (with-eval-after-load 'evil
    (evil-define-key 'insert markdown-mode-map
      (kbd "<tab>") #'completion-at-point))
  (defun +markdown/preview-toggle ()
    (interactive)
    (if (eq major-mode 'markdown-mode)
        (markdown-view-mode)
      (markdown-mode))))

(provide 'init-markdown)
