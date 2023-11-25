(tool-bar-mode -1)
(menu-bar-mode -1)

(use-package scroll-bar
  :ensure nil
  :config
  (message "scroll-bar is loaded")
  (set-scroll-bar-mode nil))

(use-package ace-window
  :pin melpa
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

(use-package beframe
  :when (display-graphic-p)
  :config
  (message "beframe is loaded")
  (beframe-mode 1)
  :bind-keymap ("C-c b" . beframe-prefix-map))

(provide 'init-gui-frames)
