(use-package tmr
  :config
  (message "tmr is loaded")

  ;; embark actions
  (defvar tmr-action-map
    (let ((map (make-sparse-keymap)))
      (define-key map "c" #'tmr-clone)
      (define-key map "e" #'tmr-edit-description)
      (define-key map "k" #'tmr-cancel)
      (define-key map "r" #'tmr-remove)
      (define-key map "s" #'tmr-reschedule)
      map))

  (with-eval-after-load 'embark
    (add-to-list 'embark-keymap-alist '(tmr-timer . tmr-action-map))
    (cl-loop
     for cmd the key-bindings of tmr-action-map
     if (commandp cmd) do
     (add-to-list 'embark-post-action-hooks (list cmd 'embark--restart))))

  (defun +tmr/list-active-timers ()
    (interactive)
    (list (tmr--read-timer "Timer: " t)))

  (defun +tmr/notification-notify (timer)
    (let ((title "TMR May Ring (Emacs tmr package)")
          (body (tmr--long-description-for-finished-timer timer)))
      (+macos/notify title body)))

  :custom
  (tmr-timer-finished-functions
   '(tmr-print-message-for-finished-timer
     +tmr/notification-notify))

  :bind (("C-c t t" . tmr)
         ("C-c t T" . tmr-with-description)
         ("C-c t l" . +tmr/list-active-timers)
         ("C-c t L" . tmr-tabulated-view)))

(provide 'init-timer)
