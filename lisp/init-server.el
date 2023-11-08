(use-package server
  :when (display-graphic-p)
  :ensure nil
  :defer 1
  :init
  (setq server-name "gui")
  :config
  (unless (server-running-p)
    (server-start)))

(provide 'init-server)
