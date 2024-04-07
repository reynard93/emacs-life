(use-package exec-path-from-shell
  :pin nongnu
  :if (display-graphic-p)
  :config
  (message "exec-path-from-shell is loaded")
  (exec-path-from-shell-initialize))

(use-package envrc
  :pin melpa
  :config
  (message "envrc is loaded")
  (envrc-global-mode 1))

(provide 'init-env)
