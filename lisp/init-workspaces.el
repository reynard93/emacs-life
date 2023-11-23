(use-package tab-bar
  :ensure nil
  :when (display-graphic-p)
  :config
  (message "tab-bar is loaded")

  ;; bind s-1 through s-9 to switch tabs
  (dolist (i (number-sequence 1 9))
    (global-set-key (kbd (format "s-%d" i))
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
         ("s-g" . tab-switch)
         ("s-G" . tab-group)
         ("s-{" . tab-previous)
         ("s-}" . tab-next)
         ("s-}" . tab-next)
         ("C-<tab>" . tab-recent)))

(provide 'init-workspaces)
