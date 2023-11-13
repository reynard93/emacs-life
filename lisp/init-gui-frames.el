(tool-bar-mode -1)
(menu-bar-mode -1)

(use-package scroll-bar
  :ensure nil
  :config
  (message "scroll-bar is loaded")
  (set-scroll-bar-mode nil))

(use-package tab-bar
  :ensure nil
  :config
  (message "tab-bar is loaded")
  :custom
  (tab-bar-show 1))

(use-package ace-window
  :config
  (message "ace-window is loaded")
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind ([remap other-window] . ace-window))

(use-package winner
  :ensure nil
  :config
  (message "winner is loaded")
  (winner-mode 1))

(provide 'init-gui-frames)
