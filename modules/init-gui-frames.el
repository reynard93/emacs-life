(use-package frame
  :ensure nil
  :if (display-graphic-p)
  :config
  (message "frame is loaded")
  :bind (("C-s-f" . toggle-frame-fullscreen)
         ("s-F" . select-frame-by-name)
         ("s-N" . make-frame)
         ("s-W" . delete-frame)))

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

(use-package beframe
  :if (display-graphic-p)
  :after consult
  :demand t
  :config
  (message "beframe is loaded")
  (beframe-mode 1)

  (defface beframe-buffer
    '((t :inherit modus-themes-fg-magenta))
    "Face for `consult' framed buffers.")

  (defun +beframe--buffer-names-sorted (&optional frame)
    (beframe-buffer-names frame :sort #'beframe-buffer-sort-visibility))

  (defvar beframe-consult-source
    `( :name     "Frame-specific buffers (current frame)"
       :narrow   ?F
       :category buffer
       :face     beframe-buffer
       :history  beframe-history
       :items    ,#'+beframe--buffer-names-sorted
       :action   ,#'switch-to-buffer
       :state    ,#'consult--buffer-state))

  (add-to-list 'consult-buffer-sources 'beframe-consult-source)

  :custom
  (beframe-rename-function nil)
  (beframe-create-frame-scratch-buffer nil)

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
