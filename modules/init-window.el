(use-package winner
  :ensure nil
  :config
  (message "winner is loaded")
  (winner-mode 1))

(use-package ace-window
  :pin melpa
  :config
  (message "ace-window is loaded")
  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind ([remap other-window] . ace-window))

(use-package popper
  :defer t
  :config
  (message "popper is loaded")
  (popper-mode 1)
  (popper-echo-mode 1)

  :custom
  (popper-group-function #'popper-group-by-project)
  (popper-display-control nil)
  (popper-reference-buffers
   '(compilation-mode
     rspec-compilation-mode))

  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type)))

(use-package shackle
  :pin melpa
  :config
  (message "shackle is loaded")
  (shackle-mode 1)
  :custom
  (shackle-inhibit-window-quit-on-same-windows t)
  (shackle-default-size 0.4)
  (shackle-rules
   `((calendar-mode :align below)
     (help-mode :select t)
     (magit-status-mode :same t)
     (magit-log-mode :popup t)
     (magit-log-select-mode :same t)
     (magit-revision-mode :popup t)
     (mu4e-main-mode :same t)
     (mu4e-headers-mode :same t)
     (osx-dictionary-mode :align below :size 0.3)
     (shell-mode :align below :size 0.3)
     ("*Org Select*" :align below)
     ("^CAPTURE-.+\\.org$" :regexp t :align below)
     ((:custom
       ,(lambda (buffer)
          (with-current-buffer buffer
            (bound-and-true-p gptel-mode))))
      :align right :size 0.5))))

(provide 'init-window)
