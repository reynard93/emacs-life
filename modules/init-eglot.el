;;; -*- lexical-binding: t -*-

(use-package eglot
  :ensure nil
  :after exec-path-from-shell
  :hook
  ;; Enable for common languages
  ((c-ts-mode c++-ts-mode rust-ts-mode ruby-mode ruby-ts-mode) . eglot-ensure)
  :custom
  (eglot-ignored-server-capabilities '(:window showMessage)) ; Don't disable completion
  (eglot-connect-timeout 10) ; Faster connection timeout
  (eglot-sync-connect nil) ; Asynchronous connection for better startup
  (eglot-autoshutdown t) ; Automatically shutdown server when not needed
  (eglot-events-buffer-size 0) ; Disable events buffer for performance
  (eglot-use-plists t) ; Use plists for better performance
  :config
  ;; Helper function to find ruby-lsp executable
  (dolist (mode '(((c-ts-mode c++-ts-mode) . ("clangd"
                                              "--background-index"
                                              "--clang-tidy"
                                              "--completion-style=detailed"
                                              "-j=8"
                                              "--pch-storage=memory"
                                              "--log=error"))
                  ((rust-ts-mode) . ("rust-analyzer"))
                  ; ((ruby-mode ruby-ts-mode) . (my-eglot-ruby-lsp-command))))
    (add-to-list 'eglot-server-programs mode))))

  ;; Fix completion ordering and behavior
  (setq completion-category-overrides '((eglot (styles orderless flex)))))

;; https://github.com/jdtsmith/eglot-booster (i installed with M-x package-vc-install
;; both lsp-mode and eglot uses this -> https://github.com/blahgeek/emacs-lsp-booster
(use-package eglot-booster
  :ensure (:host github :url "https://github.com/jdtsmith/eglot-booster")
  :defer t
  :after eglot
  :config (eglot-booster-mode))

(use-package consult-eglot
  :ensure t
  :after (eglot consult)
  :bind (:map eglot-mode-map
            ("C-M-." . consult-eglot-symbol
            ("C-c l r" . consult-eglot-references)
            ("C-c l a" . consult-eglot-code-actions))))

(provide 'init-eglot)
