(use-package exec-path-from-shell
  :pin nongnu
  :if (display-graphic-p)
  :config
  (message "exec-path-from-shell is loaded")
  (exec-path-from-shell-initialize))

(provide 'init-exec-path)
