;;; -*- lexical-binding: t -*-

;; not sure if i need these lines or not, was in init-dired of centaur emacs
(use-package emacs
  :ensure nil
  :init
  (setq-default indent-tabs-mode nil
                tab-width 4
                fill-column 80)
  :custom
  (scroll-margin 2)
  (use-dialog-box nil)
  (use-short-answers t)
  (ring-bell-function 'ignore)
  (confirm-kill-emacs 'y-or-n-p)
  (truncate-string-ellipsis "…")
  (select-enable-clipboard t)

  ;; Disable completion func, try 'cape-dict'
  (text-mode-ispell-word-completion nil)
  ;; Editing
  (require-final-newline t)

  ;; File
  (delete-by-moving-to-trash t)
  (create-lockfiles nil)
  (make-backup-files nil)
  (auto-save-interval 2400)
  (auto-save-timeout 300)
  (auto-save-file-name-transforms
   `((".*" ,temporary-file-directory t)))

  ;; Keyboard
  (echo-keystrokes 0.25)

  ;; Password
  (password-cache-expiry nil)

  ;; Startup
  (inhibit-startup-screen t)
  (initial-scratch-message nil)
  (initial-major-mode 'text-mode)

  ;; Version control
  (vc-follow-symlinks t)

  ;; Window
  (window-combination-resize t)

  ;; Xref
  (xref-search-program 'ripgrep)

  :config
  (save-place-mode 1)
  (blink-cursor-mode -1)
  (global-subword-mode 1))

(use-package emacs
  :ensure nil
  :bind-keymap ("M-r" . ctl-x-r-map)
  :bind
  (("C-c y" . my/yank-file-path)
   ("C-c Y" . my/yank-file-path-relative-to-project)
   ("C-c D" . my/delete-this-file)
   ("C-c R" . my/rename-this-file))
  :config
  (defun my/yank-file-path (&optional buffer dir)
    "Save the buffer path into the kill-ring.
If BUFFER is not nil, find filename of BUFFER, otherwise, find
filename of `current-buffer'. If DIR is not nil, get a relative
file path, otherwise, get a full file path with
`abbreviate-file-name'."
    (interactive)
    (if-let* ((filename (if buffer
                            (buffer-file-name buffer)
                          (buffer-file-name)))
              (path (if dir
                        (file-relative-name filename dir)
                      (abbreviate-file-name filename))))
        (progn
          (kill-new path)
          (message "Copied path: %s" path))
      (user-error "Buffer is not visiting any file")))

  (defun my/yank-file-path-relative-to-project ()
    "Save the relative buffer path into the kill-ring.
The path is relative to `project-current'."
    (interactive)
    (let ((project-root-dir
           (condition-case nil
               (project-root (project-current))
             (error nil))))
      (my/yank-file-path nil project-root-dir)))

  (defun my/delete-this-file ()
    "Kill the current buffer and deletes the file it is visiting."
    (interactive)
    (unless (and buffer-file-name (file-exists-p buffer-file-name))
      (user-error "Buffer is not visiting any file"))
    (when-let* ((buffer (current-buffer))
                (filename (buffer-file-name buffer))
                (path (abbreviate-file-name filename)))
      (when (y-or-n-p (format "Really delete %s? " path))
        (move-file-to-trash path)
        (kill-buffer buffer)
        (message "Deleted %s" path))))

  (defun my/rename-this-file (new-path)
    "Rename the current file to NEW-PATH."
    (interactive (list (read-file-name "Rename file to: ")))
    (unless (and buffer-file-name (file-exists-p buffer-file-name))
      (user-error "Buffer is not visiting any file"))
    (let ((old-path (buffer-file-name (buffer-base-buffer)))
          (new-path (expand-file-name new-path)))
      (when (directory-name-p new-path)
        (setq new-path (concat new-path (file-name-nondirectory old-path))))
      (make-directory (file-name-directory new-path) 't)
      (rename-file old-path new-path)
      (set-visited-file-name new-path t t)
      (message "Renamed to %s" (abbreviate-file-name new-path)))))

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

(use-package undo-fu-session
  :init
  (setq undo-limit (* 64 1024 1024)
        undo-strong-limit (* 96 1024 1024)
        undo-outer-limit (* 960 1024 1024))
  :hook (prog-mode text-mode conf-mode)
  :custom
  (undo-fu-session-compression 'zst))

(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h x" . helpful-command)))

(use-package crux
  :bind
  (([remap move-beginning-of-line] . crux-move-beginning-of-line)
   ([remap kill-whole-line] . crux-kill-whole-line)
   ("C-c d" . crux-duplicate-current-line-or-region)
   ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
   ("s-k" . crux-smart-kill-line)
   ("C-<return>" . crux-smart-open-line)
   ("s-j" . crux-top-join-line)
   ("C-^" . crux-switch-to-previous-buffer)
   ("s-n" . crux-create-scratch-buffer)))

(use-package doc-view
  :ensure nil
  :custom
  (large-file-warning-threshold (* 50 (expt 2 20)))
  (doc-view-mupdf-use-svg t)
  (doc-view-resolution 300))

;; Directory operations
(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
         ("C-c C-p" . wdired-change-to-wdired-mode))
  :config
  ;; Guess a default target directory
  (setq dired-dwim-target t)
  (define-key dired-mode-map "z" (lambda () (interactive) (let ((files (dired-get-marked-files)))
                                                       (with-temp-buffer 
                                                        (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
                                                         (message
                                                          "Size of all marked files: %s"
                                                          (progn
                                                            (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
                                                            (match-string 1)))))))

  ;; Always delete and copy recursively
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always)

  ;; Show directory first
  (setq dired-listing-switches "-alh --group-directories-first")

  (when (eq system-type 'darwin)
    (if (executable-find "gls")
        (progn
          ;; Use GNU ls as `gls' from `coreutils' if available.
          (setq insert-directory-program "gls")
          ;; Using `insert-directory-program'
          (setq ls-lisp-use-insert-directory-program t))
      (progn
        ;; Suppress the warning: `ls does not support --dired'.
        (setq dired-use-ls-dired nil)
        (setq dired-listing-switches "-alh"))))

  ;; very useful for finding what to delete st from dired
  (use-package dired-preview
    :hook (dired-mode . dired-preview-mode))
  ;; Show git info in dired
  (use-package dired-git-info
    :bind (:map dired-mode-map
           (")" . dired-git-info-mode)))

  ;; Allow rsync from dired buffers
  (use-package dired-rsync
    :bind (:map dired-mode-map
           ("C-c C-r" . dired-rsync)))
 )

;; `find-dired' alternative using `fd'
(when (executable-find "fd")
  (use-package fd-dired))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind
  ( :map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle)
    ("<backtab>" . dired-subtree-remove)
    ("S-TAB" . dired-subtree-remove))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package delsel
  :ensure nil
  :config
  (delete-selection-mode 1))

;; this one is so good
(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

;; Unbind C-z and C-x C-z from suspend-frame
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; I consider this a must and better default
(use-package which-key
  :ensure nil
  :diminish
  :functions
  which-key-mode
  :config
  (which-key-mode))

(use-package treemacs
  :bind
  (("C-c t t" . treemacs))
  :custom
  (treemacs-no-png-images t)
  :config
  (treemacs-project-follow-mode 1))

(provide 'init-better-defaults)
