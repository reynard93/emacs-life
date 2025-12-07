;;; -*- lexical-binding: t -*-

;; Eglot - built-in LSP client
;; Requires mise.el to be loaded first for proper PATH setup
(use-package eglot
  :ensure nil
  :custom
  (eglot-connect-timeout 30)            ; Allow more time for LSP startup
  (eglot-sync-connect nil)              ; Asynchronous connection for better startup
  (eglot-autoshutdown t)                ; Automatically shutdown server when not needed
  (eglot-events-buffer-size 0)          ; Disable events buffer for performance
  :config
  ;; Ruby LSP configuration
  ;; ruby-lsp is the default server for ruby-mode in Eglot 1.12+
  ;; It will be found via PATH set by mise.el
  (setq completion-category-overrides '((eglot (styles orderless flex))))

  ;; Start eglot after mise-mode has set up the environment
  ;; This ensures ruby-lsp is found in the mise-managed PATH
  (defun my/eglot-ensure-after-mise ()
    "Start eglot after mise-mode has updated the buffer environment."
    (when (and (bound-and-true-p mise-mode)
               (derived-mode-p 'ruby-mode 'ruby-ts-mode))
      (eglot-ensure)))

  ;; Hook into mise-mode to start eglot after environment is set
  (add-hook 'mise-mode-hook #'my/eglot-ensure-after-mise)

  ;; Also try to start eglot when entering ruby modes if mise is already active
  (dolist (mode '(ruby-mode-hook ruby-ts-mode-hook))
    (add-hook mode
              (lambda ()
                (if (bound-and-true-p mise-mode)
                    (eglot-ensure)
                  ;; If mise-mode isn't active yet, wait for it
                  (add-hook 'mise-mode-hook #'my/eglot-ensure-after-mise nil t))))))

;; https://github.com/jdtsmith/eglot-booster
;; Boosts eglot performance using emacs-lsp-booster
;; Install: https://github.com/blahgeek/emacs-lsp-booster
(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

(use-package consult-eglot
  :ensure t
  :after (eglot consult)
  :bind (:map eglot-mode-map
              ("C-M-." . consult-eglot-symbols)
              ("C-c l r" . consult-eglot-references)
              ("C-c l a" . consult-eglot-code-actions)))

(provide 'init-eglot)
