(when (display-graphic-p)
  (bind-key "s-s" #'save-buffer)               ; File -> Save
  (bind-key "s-S" #'write-file)                ; File -> Save As
  (bind-key "s-a" #'mark-whole-buffer)         ; File -> Select All
  (bind-key "s-c" #'kill-ring-save)            ; Edit -> Copy
  (bind-key "s-v" #'yank)                      ; Edit -> Paste
  (bind-key "s-x" #'kill-region)               ; Edit -> Cut
  (bind-key "s-z" #'undo)                      ; Edit -> Undo
  (bind-key "s-Z" #'undo-redo))                ; Edit -> Redo

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
         ("s-{" . tab-previous)
         ("s-}" . tab-next)))

(use-package frame
  :ensure nil
  :if (display-graphic-p)
  :config
  (message "frame is loaded")
  (defun tab-close-or-delete-frame ()
    "Close the current tab if there are multiple tabs, otherwise delete the frame."
    (interactive)
    (if (and (bound-and-true-p tab-bar-mode)
             (> (length (tab-bar-tabs)) 1))
        (tab-close)
      (delete-frame)))
  :bind (("C-s-f" . toggle-frame-fullscreen)
         ("s-w" . tab-close-or-delete-frame)))

(use-package beframe
  :if (display-graphic-p)
  :demand t
  :config
  (message "beframe is loaded")
  (beframe-mode 1)

  (defvar consult-buffer-sources)
  (declare-function consult--buffer-state "consult")

  (with-eval-after-load 'consult
    (defface beframe-buffer
      '((t :inherit font-lock-string-face))
      "Face for `consult' framed buffers.")

    (defun +beframe--buffer-names-sorted (&optional frame)
      "Return the list of buffers from `beframe-buffer-names' sorted by visibility.
With optional argument FRAME, return the list of buffers of FRAME."
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

    (add-to-list 'consult-buffer-sources 'beframe-consult-source :append))

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
