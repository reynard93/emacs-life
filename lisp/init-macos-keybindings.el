(setq mac-command-modifier      'super
      mac-option-modifier       'meta
      mac-right-option-modifier 'none)

(bind-key "C-s-f" #'toggle-frame-fullscreen) ; Toggle fullscreen
(bind-key "s-x" #'execute-extended-command)  ; M-x
(bind-key "s-s" #'save-buffer)               ; File -> Save
(bind-key "s-S" #'write-file)                ; File -> Save As
(bind-key "s-a" #'mark-whole-buffer)         ; File -> Select All
(bind-key "s-c" #'kill-ring-save)            ; Edit -> Copy
(bind-key "s-v" #'yank)                      ; Edit -> Paste
(bind-key "s-z" #'undo)                      ; Edit -> Undo

(provide 'init-macos-keybindings)
