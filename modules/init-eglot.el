;;; -*- lexical-binding: t -*-

;; Eglot - built-in LSP client
;; Requires mise.el to be loaded first for proper PATH setup
(use-package eglot
  :ensure nil
  :custom
  (eglot-connect-timeout 30)            ; Allow more time for LSP startup
  (eglot-sync-connect nil)              ; Asynchronous connection for better startup
  (eglot-autoshutdown t)                ; Automatically shutdown server when not needed
  :config
  ;; Ruby LSP configuration
  ;; ruby-lsp is the default server for ruby-mode in Eglot 1.12+
  ;; It will be found via PATH set by mise.el
  (setq completion-category-overrides '((eglot (styles orderless flex))))

  ;; Configure Ruby LSP server to use ruby-lsp from mise's PATH.
  ;; Since ruby-lsp is installed via mise (not in Gemfile), we use it directly
  ;; rather than through bundle exec. mise.el ensures the correct PATH is set.
  (defun my/eglot-ruby-lsp-contact (&optional _interactive)
    "Return the command to start ruby-lsp from mise's PATH."
    '("ruby-lsp"))

  (add-to-list 'eglot-server-programs
               '((ruby-mode ruby-ts-mode) . my/eglot-ruby-lsp-contact))

  ;; Disable automatic reconnection to prevent reconnection loops.
  ;; Manual reconnection available via: M-x eglot
  (setq eglot-autoreconnect nil)

  ;; Auto-start for Ruby buffers, but without "try once and never again" behavior.
  ;; We defer start until mise-mode has had a chance to set buffer-local env.
  (defvar-local my/eglot-ruby--pending nil
    "Non-nil means a Ruby Eglot start has been queued for this buffer.")

  (defun my/eglot--ensure-ruby ()
    "Ensure Eglot is running in the current Ruby buffer."
    (when (and buffer-file-name
               (buffer-live-p (current-buffer))
               (derived-mode-p 'ruby-mode 'ruby-ts-mode)
               (not (derived-mode-p 'treemacs-mode))
               (not (eglot-current-server)))
      (condition-case err
          (eglot-ensure)
        (error
         (message "[Eglot] Ruby start failed (retry manually with M-x eglot): %s"
                  (error-message-string err))))))

  (defun my/eglot-ensure-ruby-after-env ()
    "Queue Eglot start for Ruby after mise-mode/environment is ready."
    (unless my/eglot-ruby--pending
      (setq my/eglot-ruby--pending t)
      (let ((buf (current-buffer)))
        (if (bound-and-true-p mise-mode)
            (run-at-time 0 nil (lambda () (when (buffer-live-p buf)
                                            (with-current-buffer buf
                                              (setq my/eglot-ruby--pending nil)
                                              (my/eglot--ensure-ruby)))))
          ;; If mise-mode isn't enabled yet, wait for it *in this buffer only*.
          (add-hook 'mise-mode-hook
                    (lambda ()
                      (when (buffer-live-p buf)
                        (with-current-buffer buf
                          (setq my/eglot-ruby--pending nil)
                          (my/eglot--ensure-ruby))))
                    nil t)))))

  (dolist (mode '(ruby-mode-hook ruby-ts-mode-hook))
    (add-hook mode #'my/eglot-ensure-ruby-after-env)))

;; https://github.com/jdtsmith/eglot-booster
;; Boosts eglot performance using emacs-lsp-booster
;; Install: https://github.com/blahgeek/emacs-lsp-booster
(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :defer t  ; Defer loading to prevent xref from loading before Elpaca
  :hook (eglot-managed-mode . eglot-booster-mode))

(provide 'init-eglot)
