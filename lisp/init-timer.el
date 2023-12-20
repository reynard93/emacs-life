(use-package tmr
  :config
  (message "tmr is loaded")
  :custom
  (tmr-timer-finished-functions '(tmr-print-message-for-finished-timer))
  :bind (("C-c t t" . tmr)
         ("C-c t T" . tmr-with-description)
         ("C-c t e" . tmr-edit-description)
         ("C-c t l" . tmr-tabulated-view)
         ("C-c t c" . tmr-clone)
         ("C-c t k" . tmr-cancel)
         ("C-c t s" . tmr-reschedule)
         ("C-c t r" . tmr-remove)
         ("C-c t R" . tmr-remove-finished)))

(provide 'init-timer)
