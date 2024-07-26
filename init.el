;; Making custom-file disposable
(setq custom-file (make-temp-file "emacs-custom-"))

;; Loading modules
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Bootstrap
(require 'init-elpa)
(require 'init-exec-path)

;; Lisp
(require 'init-lisp)

;; Interaction
(require 'init-minibuffer)
(require 'init-completion)
(require 'init-transient)

;; Visualization
(require 'init-ui)
(require 'init-ux)
(require 'init-pulse)
(require 'init-mode-line)

;; Common
(require 'init-better-defaults)
(require 'init-editing-utils)
(require 'init-gui-frames)
(require 'init-windows)

;; Agenda/Notes
(require 'init-org)
(require 'init-denote)

;; Programming
(require 'init-tree-sitter)
(require 'init-eglot)
(require 'init-data)
(require 'init-direnv)
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
