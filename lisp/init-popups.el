(use-package popper
  :demand t
  :config
  (message "popper is loaded")
  (popper-mode 1)
  (popper-echo-mode 1)

  :custom
  (popper-group-function #'popper-group-by-project)
  (popper-display-control nil)
  (popper-reference-buffers
   '(compilation-mode
     rspec-compilation-mode
     shell-mode))

  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type)))

(use-package shackle
  :pin melpa
  :config
  (message "shackle is loaded")
  (shackle-mode 1)
  :custom
  (shackle-rules
   `((magit-status-mode :same t)
     (osx-dictionary-mode :align below :size 0.3)
     ((:custom
       ,(lambda (buffer)
          (with-current-buffer buffer
            (bound-and-true-p gptel-mode))))
      :align right :size 0.5)))
  (shackle-default-rule
   '(:align below :size 0.4)))

(provide 'init-popups)
