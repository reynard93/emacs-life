;; Window
(use-package window
  :ensure nil
  :bind ("M-o" . other-window))

;; default-keybindings is S instead of s (interferes with calendar selection)
(use-package windmove
  :ensure nil
  :bind
  (("s-<up>" . windmove-up)
   ("s-<down>" . windmove-down)
   ("s-<left>" . windmove-left)
   ("s-<right>" . windmove-right))
  :config
  (windmove-delete-default-keybindings)
  (windmove-swap-states-default-keybindings))

(use-package winner
  :ensure nil
  :config
  (winner-mode 1))

(use-package transpose-frame
  :bind
  (:map window-prefix-map
        ("w" . transpose-frame)
        ("r" . rotate-frame-clockwise)
        ("R" . rotate-frame-anticlockwise)))

(use-package ace-window
  :init
  ;; Open any buffer by splitting any window
  ;; https://karthinks.com/software/fifteen-ways-to-use-embark/#open-any-buffer-by-splitting-any-window
  (eval-when-compile
    (defmacro embark-aw-action (fn)
      `(defun ,(intern (concat "embark-aw-" (symbol-name fn))) ()
         ,(format "Open %s buffer selected with ace-window." (symbol-name fn))
         (interactive)
         (with-demoted-errors "%s"
           (require 'ace-window)
           (let ((aw-dispatch-always t))
             (aw-switch-to-window (aw-select nil))
             (call-interactively (symbol-function ',fn)))))))

  (defvar embark-aw-find-file (embark-aw-action find-file))
  (defvar embark-aw-switch-to-buffer (embark-aw-action switch-to-buffer))
  (defvar embark-aw-bookmark-jump (embark-aw-action bookmark-jump))

  :bind
  (("C-x o" . ace-window)
   :map embark-file-map
   ("o" . embark-aw-find-file)
   :map embark-buffer-map
   ("o" . embark-aw-switch-to-buffer)
   :map embark-bookmark-map
   ("o" . embark-aw-bookmark-jump))

  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package popper
  :init
  (setq popper-reference-buffers
        (append '(compilation-mode
                  rspec-compilation-mode
                  exunit-compilation-mode
                  inf-ruby-mode
                  devdocs-mode)
                '("^\\*Messages\\*$"
                  ("^\\*Async Shell Command\\*$" . hide)
                  "^\\*Shell Command Output\\*$")))
  :hook emacs-startup
  :bind
  (("C-`"   . popper-toggle)
   ("M-`"   . popper-cycle)
   ("C-M-`" . popper-toggle-type))
  :custom
  (popper-group-function #'selected-frame)
  (popper-display-control 'user))

(use-package popper-echo
  :ensure nil
  :after popper
  :config
  (popper-echo-mode 1))

(use-package shackle
  :custom
  (shackle-inhibit-window-quit-on-same-windows t)
  (shackle-rules
   `((calendar-mode :align below)
     ;; Org capture windows
     ("*Org Select*" :align below)
     ("*Capture*" :align below)
     ("^CAPTURE-.+$" :regexp t :align below)
     ;; Modes
     (calibre-library-mode :select t :same t)))
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

(use-package beframe
  :if (display-graphic-p)
  :hook emacs-startup
  :bind-keymap ("C-c b" . beframe-prefix-map)
  :custom
  (beframe-functions-in-frames '(project-prompt-project-dir)))

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
  :config
  (require 'init-avy))

(use-package move-text
  :bind
  (("M-<up>" . move-text-up)
   ("M-<down>" . move-text-down)))

(defun cc/move-word-backward ()
  "Move word to the right of point backward one word.
Point must be at the beginning of word."
  (interactive)
  (transpose-words 1)
  (forward-word -2))

(defun cc/move-word-forward ()
  "Move word to the right of point forward one word.
Point must be at the beginning of word."
  (interactive)
  (forward-word 1)
  (transpose-words 1)
  (forward-word -1))

(defun cc/move-sentence-backward ()
  "Move sentence to the right of point backward one sentence.
Point must be at the beginning of sentence."
  (interactive)
  (transpose-sentences 1)
  (forward-sentence -2))

(defun cc/move-sentence-forward ()
  "Move sentence to the right of point forward one sentence.
Point must be at the beginning of sentence."
  (interactive)
  (forward-sentence 1)
  (transpose-sentences 1)
  (forward-sentence -1))

(defun cc/move-sexp-backward ()
  "Move balanced expression (sexp) to the right of point backward one sexp.
Point must be at the beginning of balanced expression (sexp)."
  (interactive)
  (transpose-sexps 1)
  (forward-sexp -2))

(defun cc/move-sexp-forward ()
  "Move balanced expression (sexp) to the right of point forward one sexp.
Point must be at the beginning of balanced expression (sexp)."
  (interactive)
  (forward-sexp 1)
  (transpose-sexps 1)
  (forward-sexp -1))


(provide 'init-movement-utils)
