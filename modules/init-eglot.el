;;; -*- lexical-binding: t -*-

(use-package eglot
  :ensure nil
  :hook
  ((ruby-mode ruby-ts-mode) . eglot-ensure)
  ((python-mode python-ts-mode) . eglot-ensure)
  ((elixir-ts-mode heex-ts-mode) . eglot-ensure)
  (nix-mode . eglot-ensure)
  (c-ts-mode . eglot-ensure)
  :custom
  (eglot-ignored-server-capabilities '(:completionProvider)) ; using corfu,corfu-tempel
  (eglot-connect-timeout 10) ; Faster connection timeout
  (eglot-sync-connect nil) ; Asynchronous connection for better startup
  (eglot-autoshutdown t) ; Automatically shutdown server when not needed
  (eglot-events-buffer-size 0) ; Disable events buffer for performance
  :config
  (dolist (mode '(((ruby-mode ruby-ts-mode) . ("ruby-lsp"))
                  ((python-mode python-ts-mode) . ("ruff" "server"))
                  ((elixir-ts-mode heex-ts-mode) . ("nextls" "--stdio=true"))
                  (nix-mode . ("nixd"))
                  (c-ts-mode . ("clangd"
                                ;; feature options
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                ;; misc options
                                "-j=8"
                                "--pch-storage=memory"
                                ;; logging options
                                "--log=error"
                                ))))
    (add-to-list 'eglot-server-programs mode)))

;; https://github.com/jdtsmith/eglot-booster (i installed with M-x package-vc-installl
;; both lsp-mode and eglot uses this -> https://github.com/blahgeek/emacs-lsp-booster
(use-package eglot-booster
  :ensure (:host github :url "https://github.com/jdtsmith/eglot-booster")
  :defer t
  :after eglot
  :config (eglot-booster-mode))

(provide 'init-eglot)
