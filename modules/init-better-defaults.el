(recentf-mode 1)
(save-place-mode 1)
(blink-cursor-mode -1)
(global-subword-mode 1)

(setq-default delete-by-moving-to-trash t         ; Delete files to trash
              window-combination-resize t         ; take new window space from all other windows (not just current)
              x-stretch-cursor t                  ; Stretch cursor to the glyph width
              indent-tabs-mode nil
              fill-column 80)

(setq auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"               ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                   ; I can trust my computers ... can't I?
      scroll-margin 2                             ; It's nice to maintain a little margin
      display-time-default-load-average nil       ; I don't think I've ever found this useful
      use-short-answers t                         ; Prefer short anwsers
      require-final-newline t                     ; Save file with a trailing newline
      inhibit-startup-screen t                    ; Skip the startup screen
      initial-scratch-message nil                 ; Blank the *scratch* buffer
      use-dialog-box nil                          ; Disable dialog windows
      confirm-kill-emacs 'y-or-n-p                ; Confirm when leaving Emacs
      ring-bell-function 'ignore                  ; Disable ring bell
      create-lockfiles nil                        ; Avoid creating ".#filename"
      make-backup-files nil)                      ; Avoid creating "filename~"

(setq ns-use-native-fullscreen nil
      native-comp-async-report-warnings-errors 'silent
      initial-major-mode 'fundamental-mode)

(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1)
  :custom
  (history-length 500)
  (history-delete-duplicates t))

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

(use-package undo-fu
  :pin melpa
  :init
  (setq undo-limit 67108864             ; 64mb
        undo-strong-limit 100663296     ; 96mb
        undo-outer-limit 1006632960))   ; 960mb

(use-package undo-fu-session
  :pin melpa
  :after undo-fu
  :init
  (undo-fu-session-global-mode 1)
  :custom
  (undo-fu-session-compression 'zst))

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
         ("s-o" . crux-smart-open-line-above)
         ("M-o" . crux-smart-open-line)
         ("C-^" . crux-switch-to-previous-buffer)
         ("s-n" . crux-create-scratch-buffer)))

(provide 'init-better-defaults)
