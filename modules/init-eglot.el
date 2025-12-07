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

  ;; Hook architecture for mise.el integration:
  ;;
  ;; STRATEGY: Use both mise-mode-hook AND Ruby mode hooks
  ;;
  ;; 1. mise-mode-hook (PRIMARY):
  ;;    - Called when mise.el finishes updating buffer environment
  ;;    - Ensures exec-path contains mise-managed Ruby bin directory
  ;;    - Function has guards to only run in Ruby file buffers
  ;;    - This is the CORRECT way to wait for mise
  ;;
  ;; 2. ruby-mode-hook / ruby-ts-mode-hook (FALLBACK):
  ;;    - Handles case where mise-mode already finished before Ruby mode activated
  ;;    - Function checks if Eglot is already running (no duplicates)
  ;;
  ;; This dual-hook approach handles all timing scenarios without race conditions.

  ;; Primary: Start Eglot after mise updates environment
  (add-hook 'mise-mode-hook #'my/eglot-ensure-safe)

  ;; Fallback: Start Eglot when entering Ruby mode (if mise already done)
  (dolist (mode '(ruby-mode-hook ruby-ts-mode-hook))
    (add-hook mode #'my/eglot-ensure-safe)))

;; https://github.com/jdtsmith/eglot-booster
;; Boosts eglot performance using emacs-lsp-booster
;; Install: https://github.com/blahgeek/emacs-lsp-booster
(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :defer t  ; Defer loading to prevent xref from loading before Elpaca
  :hook (eglot-managed-mode . eglot-booster-mode))

(provide 'init-eglot)
