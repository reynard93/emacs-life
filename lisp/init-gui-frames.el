(use-package tab-bar
  :ensure nil
  :when (display-graphic-p)
  :config
  (message "tab-bar is loaded")
  ;; bind s-1 through s-9 to switch tabs
  (dolist (i (number-sequence 1 9))
    (bind-key (format "s-%d" i)
              `(lambda ()
                 (interactive)
                 (when (<= ,i (length (tab-bar-tabs)))
                   (tab-bar-select-tab ,i)))))

  :custom
  (tab-bar-show 1)

  :bind (("s-t" . tab-new)
         ("s-T" . tab-undo)
         ("s-w" . tab-close)
         ("s-W" . tab-close-group)
         ("s-e" . tab-switch)
         ("s-{" . tab-previous)
         ("s-}" . tab-next)
         ("C-<tab>" . tab-recent)))

(use-package beframe
  :when (display-graphic-p)
  :config
  (message "beframe is loaded")
  (beframe-mode 1)
  :bind-keymap ("C-c b" . beframe-prefix-map))

(use-package server
  :ensure nil
  :when (display-graphic-p)
  :init
  (setq server-name "gui")
  :config
  (message "server is loaded")
  (unless (server-running-p)
    (server-start)))

(provide 'init-gui-frames)
