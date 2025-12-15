;;; -*- lexical-binding: t -*-

;; Eglot - built-in LSP client

(use-package eglot
  :ensure nil
  :bind ( :map eglot-mode-map
	      ("C-c l a" . eglot-code-actions)
	      ("C-c l o" . eglot-code-action-organize-imports)
	      ("C-c l q" . eglot-code-action-quickfix)
	      ("C-c l e" . eglot-code-action-extract)
	      ("C-c l r" . eglot-rename)
	      ("C-c l i" . eglot-inlay-hints-mode))
  :hook ((ruby-mode . eglot-ensure)
         (ruby-ts-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (tsx-ts-mode . eglot-ensure)
         (js-mode . eglot-ensure)
         (js-ts-mode . eglot-ensure))
  :custom
  (eglot-sync-connect 0)              ; Asynchronous connection for better startup
  (eglot-autoshutdown t)                ; Automatically shutdown server when not needed
  (eglot-autoreconnect nil)
  (eglot-events-buffer-size 0)        ; Disable logging to prevent performance issues
  :config
  (setf (alist-get 'eglot completion-category-overrides)
        '(styles orderless flex))
  (add-to-list 'eglot-server-programs
               '((ruby-mode ruby-ts-mode) . ("ruby-lsp")))


  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode tsx-ts-mode js-ts-mode js-mode)
                 . ("rass" "tsreact")))

  ;; HTML via vscode-langservers-extracted
  (add-to-list 'eglot-server-programs
               '((html-mode web-mode)
                 . ("vscode-html-language-server" "--stdio")))

  ;; CSS via vscode-langservers-extracted
  (add-to-list 'eglot-server-programs
               '((css-mode css-ts-mode)
                 . ("vscode-css-language-server" "--stdio")))

  ;; JSON via vscode-langservers-extracted
  (add-to-list 'eglot-server-programs
               '((json-mode json-ts-mode)
                 . ("vscode-json-language-server" "--stdio")))
)

;;; Web development LSP configuration (TS/JS/TSX/JSX/Tailwind/ESLint)
;; Uses rassumfrassum to multiplex multiple LSP servers for JS/TS/TSX files.
;; Preset: ~/.config/rassumfrassum/tsreact.py (vtsls + eslint-ls + tailwind-ls)
(with-eval-after-load 'eglot
  ;; Prefer external formatting (Apheleia/Prettier) instead of LSP formatting.
  (setq eglot-ignored-server-capabilities
        '(:documentFormattingProvider
          :documentRangeFormattingProvider))

  ;; Some servers dynamically register workspace folder notifications, but Eglot
  ;; doesn't implement `workspace/didChangeWorkspaceFolders'.  Don't advertise
  ;; support so servers won't try to register it.
  (cl-defmethod eglot-client-capabilities :around ()
    (let* ((caps (cl-call-next-method))
           (ws (plist-get caps :workspace)))
      (setq ws (plist-put ws :workspaceFolders :json-false))
      (setq ws (plist-put ws :didChangeWatchedFiles '(:dynamicRegistration :json-false)))
      (plist-put caps :workspace ws)
      caps)))

;; https://github.com/jdtsmith/eglot-booster
;; Boosts eglot performance using emacs-lsp-booster
;; Install: https://github.com/blahgeek/emacs-lsp-booster
(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :defer t  ; Defer loading to prevent xref from loading before Elpaca
  :hook (eglot-managed-mode . eglot-booster-mode))

(provide 'init-eglot)
