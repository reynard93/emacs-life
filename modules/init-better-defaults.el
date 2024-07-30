(use-package emacs
  :ensure nil
  :init
  (setq-default indent-tabs-mode nil
                tab-width 4
                fill-column 80)
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
  (vc-follow-symlinks t)
  :config
  (save-place-mode 1)
  (blink-cursor-mode -1)
  (global-subword-mode 1))

(use-package emacs
  :ensure nil
  :init
  (defun +buffer/yank-path (&optional buffer dir)
    "Save the buffer path into the kill-ring.
If BUFFER is not nil, find filename of BUFFER, otherwise, find
filename of `current-buffer'. If DIR is not nil, get a relative
file path, otherwise, get a full file path with
`abbreviate-file-name'."
    (interactive)
    (if-let* ((filename (if buffer
                            (buffer-filename buffer)
                          (buffer-file-name)))
              (path (if dir
                        (file-relative-name filename dir)
                      (abbreviate-file-name filename))))
        (progn
          (kill-new path)
          (message "Copied path: %s" path))
      (user-error "Buffer is not visiting any file")))

  (defun +buffer/yank-path-relative-to-project ()
    "Save the relative buffer path into the kill-ring.
The path is relative to `project-current'."
    (interactive)
    (let ((project-root-dir
           (condition-case nil
               (project-root (project-current))
             (error nil))))
      (+buffer/yank-path nil project-root-dir)))

  (defun +file/delete-this-file ()
    (interactive)
    (when-let* ((buffer (current-buffer))
                (filename (buffer-file-name buffer))
                (path (abbreviate-file-name filename)))
      (when (y-or-n-p (format "Really delete %s? " path))
        (move-file-to-trash path)
        (kill-buffer buffer)
        (message "Deleted %s" path))))

  (defun +file/move-this-file (new-path)
    (interactive (list (read-file-name "Move file to: ")))
    (unless (and buffer-file-name (file-exists-p buffer-file-name))
      (user-error "Buffer is not visiting any file"))
    (let ((old-path (buffer-file-name (buffer-base-buffer)))
          (new-path (expand-file-name new-path)))
      (when (directory-name-p new-path)
        (setq new-path (concat new-path (file-name-nondirectory old-path))))
      (make-directory (file-name-directory new-path) 't)
      (rename-file old-path new-path)
      (set-visited-file-name new-path t t)
      (message "File moved to %S" (abbreviate-file-name new-path))))

  :bind
  (("C-c y" . +buffer/yank-path)
   ("C-c Y" . +buffer/yank-path-relative-to-project)
   ("C-c D" . +file/delete-this-file)
   ("C-c R" . +file/move-this-file)))

(use-package recentf
  :ensure nil
  :custom
  (recentf-max-saved-items 200)
  :config
  (recentf-mode 1))

(use-package savehist
  :ensure nil
  :custom
  (history-length 500)
  (history-delete-duplicates t)
  :config
  (add-to-list 'savehist-additional-variables 'log-edit-comment-ring)
  (savehist-mode 1))

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
  :init
  (defun compilation-filter-colorize ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :hook (compilation-filter . compilation-filter-colorize))

(use-package undo-fu-session
  :pin melpa
  :init
  (setq undo-limit (* 64 1024 1024)
        undo-strong-limit (* 96 1024 1024)
        undo-outer-limit (* 960 1024 1024))
  :hook (prog-mode text-mode conf-mode)
  :custom
  (undo-fu-session-compression 'zst))

(use-package helpful
  :pin melpa
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h x" . helpful-command)))

(use-package crux
  :pin melpa
  :bind
  (([remap move-beginning-of-line] . crux-move-beginning-of-line)
   ([remap kill-whole-line] . crux-kill-whole-line)
   ("C-c d" . crux-duplicate-current-line-or-region)
   ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
   ("s-k" . crux-smart-kill-line)
   ("s-j" . crux-top-join-line)
   ("C-^" . crux-switch-to-previous-buffer)
   ("s-n" . crux-create-scratch-buffer)))

(provide 'init-better-defaults)
