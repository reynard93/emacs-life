;;; -*- lexical-binding: t -*-

;; Eglot - built-in LSP client
;; Requires mise.el to be loaded first for proper PATH setup

(use-package eglot
  :ensure nil
  :bind ( :map eglot-mode-map
	      ("C-c l a" . eglot-code-actions)
	      ("C-c l o" . eglot-code-action-organize-imports)
	      ("C-c l q" . eglot-code-action-quickfix)
	      ("C-c l e" . eglot-code-action-extract)
	      ("C-c l r" . eglot-rename)
	      ("C-c l i" . eglot-inlay-hints-mode))
  :custom
  (eglot-sync-connect 0)              ; Asynchronous connection for better startup
  (eglot-autoshutdown t)                ; Automatically shutdown server when not needed
  (eglot-autoreconnect nil)
  :config
  (setq completion-category-overrides '((eglot (styles orderless flex))))
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
  ;; Use rass to run multiple servers (vtsls + ESLint + Tailwind) for JS/TS/TSX/JSX.
  ;; Requires: pip install rassumfrassum
  ;; Requires: npm install -D @vtsls/language-server vscode-langservers-extracted @tailwindcss/language-server
  ;; Prefer external formatting (Apheleia/Prettier) instead of LSP formatting.
  ;; This prevents conflicts between LSP and Apheleia when formatting on save.
  ;; Also disable file watching to prevent conflicts with treemacs and reduce re-indexing.
  (setq eglot-ignored-server-capabilities
        '(:documentFormattingProvider
          :documentRangeFormattingProvider
          :workspace/didChangeWatchedFiles
          :workspace/didChangeWorkspaceFolders)))

;; https://github.com/jdtsmith/eglot-booster
;; Boosts eglot performance using emacs-lsp-booster
;; Install: https://github.com/blahgeek/emacs-lsp-booster
(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :defer t  ; Defer loading to prevent xref from loading before Elpaca
  :hook (eglot-managed-mode . eglot-booster-mode))

(provide 'init-eglot)
