;;; -*- lexical-binding: t -*-

;; latest and most maintained
(use-package verb
  :ensure t
  :config
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(provide 'init-http)
