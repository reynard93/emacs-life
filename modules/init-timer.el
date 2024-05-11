(use-package tmr
  :defer t
  :config
  (message "tmr is loaded")

  (defvar-keymap embark-tmr-map
    "c" #'tmr-clone
    "e" #'tmr-edit-description
    "r" #'tmr-remove
    "R" #'tmr-remove-finished
    "s" #'tmr-reschedule)

  (with-eval-after-load 'embark
    (add-to-list 'embark-keymap-alist '(tmr-timer . embark-tmr-map))
    (cl-loop
     for cmd the key-bindings of embark-tmr-map
     if (commandp cmd) do
     (add-to-list 'embark-post-action-hooks (list cmd 'embark--restart))))

  (defun +tmr--notification-notify (timer)
    (let ((title "TMR May Ring (Emacs tmr package)")
          (body (tmr--long-description-for-finished-timer timer)))
      (+macos/notify title body)))

  :custom
  (tmr-timer-finished-functions
   '(tmr-print-message-for-finished-timer
     +tmr--notification-notify)))

(provide 'init-timer)
