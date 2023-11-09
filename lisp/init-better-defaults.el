(setq confirm-kill-emacs 'y-or-n-p    ; Confirm when leaving Emacs
      inhibit-startup-screen t        ; Skip the startup screen
      initial-scratch-message nil     ; Display nothing on the *scratch* buffer
      create-lockfiles nil            ; Avoid creating ".#filename"
      make-backup-files nil)          ; Avoid creating "filename~"

(use-package recentf
  :config
  (message "recentf is loaded")
  (recentf-mode 1))

(use-package savehist
  :config
  (message "savehist is loaded")
  (savehist-mode 1))

(use-package helpful
  :pin melpa
  :config
  (message "helpful is loaded")
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)))

;; https://tecosaur.github.io/emacs-config/config.html#better-defaults
(setq-default delete-by-moving-to-trash t         ; Delete files to trash
              window-combination-resize t         ; take new window space from all other windows (not just current)
              x-stretch-cursor t)                 ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                   ; I can trust my computers ... can't I?
      scroll-margin 2                             ; It's nice to maintain a little margin
      display-time-default-load-average nil       ; I don't think I've ever found this useful
      use-short-answers t                         ; Prefer short anwsers
      require-final-newline t)                    ; Save file with a trailing newline

(add-to-list 'default-frame-alist '(height . 24))
(add-to-list 'default-frame-alist '(width . 80))

(provide 'init-better-defaults)
