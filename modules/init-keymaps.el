(defvar my-insert-map (make-sparse-keymap)
  "A keymap for insert commands.")

(define-key global-map (kbd "C-c i") my-insert-map)

(provide 'init-keymaps)
