(setq mac-command-modifier      'super
      mac-option-modifier       'meta
      mac-right-option-modifier 'none)

(global-set-key (kbd "C-s-f") #'toggle-frame-fullscreen) ; Toggle fullscreen
(global-set-key (kbd "s-x") #'execute-extended-command)  ; M-x
(global-set-key (kbd "s-o") #'find-file)                 ; Open file
(global-set-key (kbd "s-s") #'save-buffer)               ; Save file
(global-set-key (kbd "s-w") #'kill-this-buffer)          ; Close file
(global-set-key (kbd "s-a") #'mark-whole-buffer)         ; Select All
(global-set-key (kbd "s-c") #'kill-ring-save)            ; Copy
(global-set-key (kbd "s-v") #'yank)                      ; Paste
(global-set-key (kbd "s-z") #'undo)                      ; Undo

(provide 'init-macos-keybindings)
