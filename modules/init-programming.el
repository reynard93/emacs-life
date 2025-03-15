(require 'init-tree-sitter)
;; Cross-referencing commands
(use-package xref
  :bind (("M-g ." . xref-find-definitions)
         ("M-g ," . xref-go-back))
  :init
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

; ;; (require 'init-eglot)
; (require 'init-lsp-proxy)
(require 'init-lsp)
(require 'init-data)
(require 'init-docker)
;; (require 'init-c)
;; (require 'init-elixir)
(require 'init-markdown)
(require 'init-nix)
(require 'init-python)
(require 'init-ruby)
(require 'init-web)
(require 'init-yaml)

(provide 'init-programming)
