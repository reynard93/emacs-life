(use-package emacs
  :ensure nil
  :bind-keymap ("M-r" . ctl-x-r-map)
  :init
  (setq-default indent-tabs-mode nil                ; Indent using spaces
                fill-column 80)                     ; Wrap lines at 80 characters
  :config
  (save-place-mode 1)
  (blink-cursor-mode -1)
  (global-subword-mode 1)
  :custom
  (delete-by-moving-to-trash t)
  (window-combination-resize t)
  (x-stretch-cursor t)
  (auto-save-default t)
  (confirm-kill-emacs 'y-or-n-p)
  (create-lockfiles nil)
  (display-time-default-load-average nil)
  (inhibit-startup-screen t)
  (initial-major-mode 'text-mode)
  (initial-scratch-message nil)
  (make-backup-files nil)
  (password-cache-expiry nil)
  (require-final-newline t)
  (ring-bell-function 'ignore)
  (scroll-margin 2)
  (truncate-string-ellipsis "…")
  (use-dialog-box nil)
  (use-short-answers t)
  (vc-follow-symlinks t))

(use-package recentf
  :ensure nil
  :config
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 200))

(use-package savehist
  :ensure nil
  :config
  (savehist-mode 1)
  :custom
  (history-length 500)
  (history-delete-duplicates t))

(use-package isearch
  :ensure nil
  :custom
  (isearch-lazy-count t)
  (isearch-repeat-on-direction-change t))

(use-package ibuffer
  :ensure nil
  :hook (ibuffer-mode . ibuffer-auto-mode)
  :bind ([remap list-buffers] . ibuffer))

(use-package ediff
  :ensure nil
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally))

(use-package compile
  :ensure nil
  :config
  (defun compilation-filter-colorize ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . compilation-filter-colorize))

(use-package undo-fu-session
  :pin melpa
  :preface
  (setq undo-limit (* 64 1024 1024)
        undo-strong-limit (* 96 1024 1024)
        undo-outer-limit (* 960 1024 1024))
  :custom
  (undo-fu-session-compression 'zst)
  :hook (prog-mode text-mode conf-mode))

(use-package helpful
  :pin melpa
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)))

(use-package crux
  :pin melpa
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ("s-k" . crux-smart-kill-line)
         ("s-j" . crux-top-join-line)
         ("C-^" . crux-switch-to-previous-buffer)
         ("s-n" . crux-create-scratch-buffer)))

(provide 'init-better-defaults)
