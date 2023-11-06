(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(use-package server
  :when (display-graphic-p)
  :ensure nil
  :defer 1
  :init
  (setq server-name "gui")
  :config
  (unless (server-running-p)
    (server-start)))

(provide 'init-gui-frames)
