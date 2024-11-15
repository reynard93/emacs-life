(defvar my-insert-map (make-sparse-keymap)
  "A keymap for insert commands.")

(define-key global-map (kbd "C-c i") my-insert-map)

(defvar my-assistant-map (make-sparse-keymap)
  "Keymap for assistant commands")

(define-key global-map (kbd "C-c a") my-assistant-map)

(provide 'init-keymaps)
