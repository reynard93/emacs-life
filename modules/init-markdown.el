(use-package markdown-mode
  :pin nongnu
  :defer t
  :config
  (message "markdown-mode is loaded")
  (with-eval-after-load 'evil
    (evil-define-key 'insert markdown-mode-map
      (kbd "<tab>") #'completion-at-point)))

(provide 'init-markdown)
