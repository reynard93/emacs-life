;; Window
(use-package window
  :ensure nil
  :bind ("M-o" . other-window))

(use-package windmove
  :ensure nil
  :config
  (windmove-default-keybindings)
  (windmove-delete-default-keybindings)
  (windmove-swap-states-default-keybindings))

(use-package winner
  :ensure nil
  :config
  (winner-mode 1))

(use-package transpose-frame
  :pin melpa
  :bind
  ( :map window-prefix-map
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
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package popper
  :bind
  (("C-`"   . popper-toggle)
   ("M-`"   . popper-cycle)
   ("C-M-`" . popper-toggle-type))
  :custom
  (popper-group-function #'popper-group-by-project)
  (popper-display-control nil)
  (popper-reference-buffers
   '(compilation-mode
     rspec-compilation-mode
     inf-ruby-mode))
  :config
  (popper-mode 1)
  (popper-echo-mode 1))

(use-package shackle
  :pin melpa
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
     (".+ (pull request)" :regexp t :align right :select t)))
  :config
  (shackle-mode 1))

;; Frame
(use-package frame
  :ensure nil
  :bind
  (("C-s-f" . toggle-frame-fullscreen)
   ("s-w" . tab-close-or-delete-frame)
   ("s-N" . make-frame))
  :config
  (defun tab-close-or-delete-frame ()
    "Close the current tab if there are multiple tabs, otherwise delete the frame."
    (interactive)
    (if (and (bound-and-true-p tab-bar-mode)
             (> (length (tab-bar-tabs)) 1))
        (tab-close)
      (delete-frame))))

;; Tab bar
(use-package tab-bar
  :ensure nil
  :bind
  (("s-t" . tab-new)
   ("s-T" . tab-undo)
   ("s-{" . tab-previous)
   ("s-}" . tab-next))
  :custom
  (tab-bar-show 1)
  :config
  ;; bind s-1 through s-9 to switch tabs
  (dolist (i (number-sequence 1 9))
    (bind-key (format "s-%d" i)
              `(lambda ()
                 (interactive)
                 (when (<= ,i (length (tab-bar-tabs)))
                   (tab-bar-select-tab ,i))))))

;; Buffer
(use-package avy
  :bind (:map goto-map ("s" . avy-goto-char-2)))

(use-package move-text
  :pin melpa
  :bind
  (("s-<up>" . move-text-up)
   ("s-<down>" . move-text-down)))

(provide 'init-movement-utils)
