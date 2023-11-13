(setq mac-command-modifier      'super
      mac-option-modifier       'meta
      mac-right-option-modifier 'none)

(global-set-key (kbd "C-s-f") #'toggle-frame-fullscreen) ; Toggle fullscreen
(global-set-key (kbd "s-x") #'execute-extended-command)  ; M-x
(global-set-key (kbd "s-o") #'find-file)                 ; Open file
(global-set-key (kbd "s-s") #'save-buffer)               ; Save file
(global-set-key (kbd "s-a") #'mark-whole-buffer)         ; Select all
(global-set-key (kbd "s-c") #'kill-ring-save)            ; Copy
(global-set-key (kbd "s-v") #'yank)                      ; Paste
(global-set-key (kbd "s-z") #'undo)                      ; Undo

(global-set-key (kbd "s-t") #'tab-new)                   ; New tab
(global-set-key (kbd "s-T") #'tab-undo)                  ; Undo tab
(global-set-key (kbd "s-w") #'tab-close)                 ; Close tab
(global-set-key (kbd "s-W") #'tab-close-group)           ; Close tab group
(global-set-key (kbd "s-g") #'tab-switch)                ; Switch to other tab
(global-set-key (kbd "s-G") #'tab-group)                 ; Add current tab to group
(global-set-key (kbd "C-<tab>") #'tab-recent)            ; Switch between latest tabs
(global-set-key (kbd "s-{") #'tab-previous)              ; Previous tab
(global-set-key (kbd "s-}") #'tab-next)                  ; Next tab

(defun yejun/move-beginning-of-line ()
  (interactive)
  (let ((orig-point (point)))
    (beginning-of-line-text)
    (when (= orig-point (point))
      (beginning-of-line))))
(define-key global-map [remap move-beginning-of-line] 'yejun/move-beginning-of-line)

(provide 'init-macos-keybindings)
