;;; -*- lexical-binding: t -*-

;; Eglot - built-in LSP client
;; Requires mise.el to be loaded first for proper PATH setup
(use-package eglot
  :ensure nil
  :custom
  (eglot-connect-timeout 30)            ; Allow more time for LSP startup
  (eglot-sync-connect nil)              ; Asynchronous connection for better startup
  (eglot-autoshutdown t)                ; Automatically shutdown server when not needed
  (eglot-events-buffer-size 0)          ; Disable events buffer for performance (set to 2000000 for debugging)
  :config
  ;; Ruby LSP configuration
  ;; ruby-lsp is the default server for ruby-mode in Eglot 1.12+
  ;; It will be found via PATH set by mise.el
  (setq completion-category-overrides '((eglot (styles orderless flex))))

  ;; Configure Ruby LSP server explicitly
  (add-to-list 'eglot-server-programs
               '((ruby-mode ruby-ts-mode) . ("ruby-lsp")))

  ;; Function to safely start Eglot after mise has set up the environment
  (defun my/eglot-ensure-safe ()
    "Safely start Eglot with proper checks and error handling.

This function is designed to work with mise.el's buffer-local environment.
It will only start Eglot when ALL conditions are met:
- Buffer is visiting a file (not scratch/special buffers)
- Buffer is alive (not killed)
- mise-mode is active (mise.el has updated buffer environment)
- We're in Ruby mode or ruby-ts-mode
- Eglot is not already running in this buffer
- Not in special buffers like treemacs

The function can be safely called multiple times - it checks if Eglot
is already running to prevent duplicate starts."
    (when (and buffer-file-name                           ; Must be a file buffer
               (buffer-live-p (current-buffer))           ; Buffer must be alive
               (bound-and-true-p mise-mode)               ; mise-mode is active
               (derived-mode-p 'ruby-mode 'ruby-ts-mode)  ; Must be Ruby mode
               (not (eglot-current-server))               ; Eglot not already running
               (not (derived-mode-p 'treemacs-mode)))     ; Not in treemacs
      (condition-case err
          (progn
            (message "[Eglot] Starting LSP for %s" (buffer-name))
            (eglot-ensure))
        (error
         (message "[Eglot] Failed to start: %s" (error-message-string err))))))

  ;; Disable automatic reconnection to prevent reconnection loops
  ;; Manual reconnection available via: M-x eglot
  (setq eglot-autoreconnect nil)

  ;; mise.el integration
  ;;
  ;; IMPORTANT: Do NOT use a global `mise-mode-hook' to start Eglot.
  ;; `global-mise-mode' enables mise-mode in every buffer, so a global hook
  ;; can fire during normal buffer creation/switching and accidentally start
  ;; Eglot from the "wrong" buffer context.
  (defun my/eglot-ensure-after-mise ()
    "Start Eglot for Ruby after mise-mode has updated this buffer's environment."
    (if (bound-and-true-p mise-mode)
        (my/eglot-ensure-safe)
      ;; If mise-mode isn't enabled yet, wait for it *in this buffer only*.
      (add-hook 'mise-mode-hook #'my/eglot-ensure-safe nil t)))

  ;; Start Eglot only for Ruby buffers.
  (dolist (mode '(ruby-mode-hook ruby-ts-mode-hook))
    (add-hook mode #'my/eglot-ensure-after-mise)))

;; https://github.com/jdtsmith/eglot-booster
;; Boosts eglot performance using emacs-lsp-booster
;; Install: https://github.com/blahgeek/emacs-lsp-booster
(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :defer t  ; Defer loading to prevent xref from loading before Elpaca
  :hook (eglot-managed-mode . eglot-booster-mode))

(provide 'init-eglot)
