(use-package tmr
  :defer t
  :commands (+tmr/pomodoro)
  :config
  (message "tmr is loaded")

  (defvar-keymap embark-tmr-map
    "c" #'tmr-clone
    "d" #'tmr-remove
    "r" #'tmr-reschedule)

  (with-eval-after-load 'embark
    (add-to-list 'embark-keymap-alist '(tmr-timer . embark-tmr-map))
    (cl-loop
     for cmd the key-bindings of embark-tmr-map
     if (commandp cmd) do
     (add-to-list 'embark-post-action-hooks (list cmd 'embark--restart))))

  (defun tmr-notification-notify (timer)
    "Override `tmr-notification-notify'. Use macOS notification."
    (let ((title "TMR May Ring (Emacs tmr package)")
          (body (tmr--long-description-for-finished-timer timer)))
      (+macos/notify title body)))

  (defun +tmr/pomodoro (arg)
    "Start a Pomodoro timer. Prompt for duration if ARG is non-nil, otherwise use 25m."
    (interactive "P")
    (let ((timer (+tmr--timer-with-description "Pomodoro"))
          (time (if arg (read-string "Time (default 25m): " nil nil "25m") "25m")))
      (if timer
          (when (yes-or-no-p (format "Cancel Pomodoro? (remaining: %s)"
                                     (tmr--format-remaining timer)))
            (tmr-cancel timer))
        (funcall-interactively #'tmr-with-description time "Pomodoro"))))

  (defun +tmr--timer-with-description (description)
    (seq-find (lambda (timer)
                (and (not (tmr--timer-finishedp timer))
                     (string= (tmr--timer-description timer) description)))
              tmr--timers)))

(provide 'init-timer)
