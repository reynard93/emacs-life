;;; -*- lexical-binding: t -*-

(require 'init-tree-sitter)
;; Cross-referencing commands
(use-package xref
  :ensure nil  ; Built-in package
  :defer t     ; Defer loading to prevent loading before Elpaca
  :bind (("M-g ." . xref-find-definitions)
         ("M-g ," . xref-go-back))
  :config
  ;; Use faster search tool
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep))

  ;; Select from xref candidates in minibuffer
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read
        xref-show-xrefs-function #'xref-show-definitions-completing-read))

;; Code styles
(use-package editorconfig
  :diminish
  :hook (after-init . editorconfig-mode))

(require 'init-eglot)
(require 'init-data)
(require 'init-docker)
(require 'init-ruby)
(require 'init-web)
(require 'init-yaml)

(use-package inheritenv :ensure)

(provide 'init-programming)
