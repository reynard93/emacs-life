(use-package winner
  :config
  (winner-mode 1))

(use-package windmove
  :config
  (windmove-default-keybindings)
  (windmove-delete-default-keybindings)
  (windmove-swap-states-default-keybindings))

(use-package transpose-frame
  :pin melpa
  :config
  :bind ( :map window-prefix-map
          ("w" . transpose-frame)
          ("r" . rotate-frame-clockwise)
          ("R" . rotate-frame-anticlockwise)))

(use-package ace-window
  :pin melpa
  :init
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
    (keymap-set embark-file-map     "o" (+embark--aw-action find-file))
    (keymap-set embark-buffer-map   "o" (+embark--aw-action switch-to-buffer))
    (keymap-set embark-bookmark-map "o" (+embark--aw-action bookmark-jump)))

  :custom
  (aw-scope 'global)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind ([remap other-window] . ace-window))

(use-package popper
  :config
  (popper-mode 1)
  (popper-echo-mode 1)
  :custom
  (popper-group-function #'popper-group-by-project)
  (popper-display-control nil)
  (popper-reference-buffers
   '(compilation-mode
     rspec-compilation-mode
     inf-ruby-mode))
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type)))

(use-package shackle
  :pin melpa
  :config
  (shackle-mode 1)
  :custom
  (shackle-inhibit-window-quit-on-same-windows t)
  (shackle-rules
   `((calendar-mode :align below)
     (osx-dictionary-mode :align below :size 0.3)
     ;; Org capture windows
     ("*Org Select*" :align below)
     ("*Capture*" :align below)
     ("^CAPTURE-.+$" :regexp t :align below)
     ;; Outputs
     (".+ (summary)" :regexp t :align below :size 0.3)
     (".+ (pull request)" :regexp t :align right :select t))))

(provide 'init-windows)
