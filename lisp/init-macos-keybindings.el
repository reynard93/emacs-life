(setq mac-command-modifier      'super
      mac-option-modifier       'meta
      mac-right-option-modifier 'none)

(bind-key "C-s-f" #'toggle-frame-fullscreen) ; Toggle fullscreen
(bind-key "s-o" #'find-file)                 ; Open file
(bind-key "s-s" #'save-buffer)               ; Save file
(bind-key "s-a" #'mark-whole-buffer)         ; Select all
(bind-key "s-c" #'kill-ring-save)            ; Copy
(bind-key "s-v" #'yank)                      ; Paste
(bind-key "s-z" #'undo)                      ; Undo

(provide 'init-macos-keybindings)
