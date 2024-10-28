(defvar my-insert-map (make-sparse-keymap)
  "A keymap for insert commands.")

(define-key global-map (kbd "C-c i") my-insert-map)

(defvar my-helper-map (make-sparse-keymap)
  "Keymap for helper commands")

(define-key global-map (kbd "C-c h") my-helper-map)

(provide 'init-keymaps)
