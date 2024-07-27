;; Adding directories to `load-path'
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Bootstrap
(require 'init-better-defaults)
(require 'init-bootstrap)
(require 'init-lisp)

;; Core
(require 'init-minibuffer)
(require 'init-completion)
(require 'init-mode-line)

;; Utils
(require 'init-better-utils)
(require 'init-movement-utils)
(require 'init-editing-utils)

;; Visualization
(require 'init-ui)
(require 'init-ux)

;; Agenda/Notes
(require 'init-org)
(require 'init-denote)

;; Programming
(require 'init-tree-sitter)
(require 'init-eglot)
(require 'init-data)
(require 'init-docker)
(require 'init-elixir)
(require 'init-markdown)
(require 'init-nix)
(require 'init-racket)
(require 'init-ruby)
(require 'init-web)
(require 'init-yaml)

;; Reading
(require 'init-elfeed)
(require 'init-wombag)
(require 'init-pdf)
(require 'init-epub)

;; Tools
(require 'init-git)
(require 'init-pass)
(require 'init-mail)
(require 'init-gpt)
(require 'init-lookup)
(require 'init-network-tools)
(require 'init-spell-checker)
