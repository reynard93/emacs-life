(when (display-graphic-p)
  (bind-key "s-x" #'execute-extended-command)  ; M-x
  (bind-key "s-s" #'save-buffer)               ; File -> Save
  (bind-key "s-S" #'write-file)                ; File -> Save As
  (bind-key "s-o" #'find-file)                 ; File -> Open File
  (bind-key "s-a" #'mark-whole-buffer)         ; File -> Select All
  (bind-key "s-c" #'kill-ring-save)            ; Edit -> Copy
  (bind-key "s-v" #'yank)                      ; Edit -> Paste
  (bind-key "s-z" #'undo))                     ; Edit -> Undo

(use-package tab-bar
  :ensure nil
  :if (display-graphic-p)
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
         ("s-g" . tab-switch)
         ("s-{" . tab-previous)
         ("s-}" . tab-next)
         ("C-<tab>" . tab-recent)))

(use-package frame
  :ensure nil
  :if (display-graphic-p)
  :config
  (message "frame is loaded")
  :bind (("C-s-f" . toggle-frame-fullscreen)
         ("s-N" . make-frame)
         ("s-W" . delete-frame)))

(use-package beframe
  :if (display-graphic-p)
  :demand t
  :config
  (message "beframe is loaded")
  (beframe-mode 1)
  :bind-keymap ("C-c b" . beframe-prefix-map))

(use-package server
  :if (display-graphic-p)
  :ensure nil
  :defer 20
  :init
  (setq server-name "gui")
  :config
  (message "server is loaded")
  (unless (server-running-p)
    (server-start)))

(provide 'init-gui-frames)
