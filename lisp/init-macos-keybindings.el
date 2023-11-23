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

(defun yejun/move-beginning-of-line ()
  (interactive)
  (let ((orig-point (point)))
    (beginning-of-line-text)
    (when (= orig-point (point))
      (beginning-of-line))))
(define-key global-map [remap move-beginning-of-line] 'yejun/move-beginning-of-line)

(provide 'init-macos-keybindings)
