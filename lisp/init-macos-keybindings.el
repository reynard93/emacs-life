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

(defun move-beginning-of-line-advice (orig-fun &rest args)
  "Advice to toggle point movement between first non-whitespace
character and beginning of line."
  (let ((orig-point (point)))
    (beginning-of-line-text)
    (when (= orig-point (point))
      (apply orig-fun args))))

(advice-add 'move-beginning-of-line :around #'move-beginning-of-line-advice)

(provide 'init-macos-keybindings)
