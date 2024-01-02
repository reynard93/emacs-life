(use-package winner
  :ensure nil
  :config
  (message "winner is loaded")
  (winner-mode 1))

(use-package ace-window
  :pin melpa
  :config
  (message "ace-window is loaded")

  ;; Open any buffer by splitting any window
  ;; https://karthinks.com/software/fifteen-ways-to-use-embark/#open-any-buffer-by-splitting-any-window
  (eval-when-compile
    (defmacro +embark--aw-action (fn)
      `(defun ,(intern (concat "+embark/aw-" (symbol-name fn))) ()
         ,(format "Open %s buffer selected with ace-window." (symbol-name fn))
         (interactive)
         (with-demoted-errors "%s"
           (require 'ace-window)
           (let ((aw-dispatch-always t))
             (aw-switch-to-window (aw-select nil))
             (call-interactively (symbol-function ',fn)))))))

  (with-eval-after-load 'embark
    (define-key embark-file-map     (kbd "o") (+embark--aw-action find-file))
    (define-key embark-buffer-map   (kbd "o") (+embark--aw-action switch-to-buffer))
    (define-key embark-bookmark-map (kbd "o") (+embark--aw-action bookmark-jump)))

  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind ([remap other-window] . ace-window))

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
   `((help-mode :select t)
     (magit-status-mode :same t)
     (magit-log-mode :popup t)
     (magit-log-select-mode :same t)
     (magit-revision-mode :popup t)
     (mu4e-main-mode :same t)
     (mu4e-headers-mode :same t)
     (osx-dictionary-mode :align below :size 0.3)
     (shell-mode :align below :size 0.3)
     ((:custom
       ,(lambda (buffer)
          (with-current-buffer buffer
            (bound-and-true-p gptel-mode))))
      :align right :size 0.5))))

(provide 'init-window)
