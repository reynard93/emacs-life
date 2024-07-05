;; Making custom-file disposable
(setq custom-file (make-temp-file "emacs-custom-"))

;; Loading modules
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Bootstrap
(require 'init-lisp)
(require 'init-elpa)
(require 'init-env)

;; Interaction
(require 'init-minibuffer)
(require 'init-completion)
(require 'init-transient)
(require 'init-embark)

;; Visualization
(require 'init-ui)
(require 'init-pulse)
(require 'init-mode-line)

;; Common
(require 'init-emacs-keybindings)
(require 'init-better-defaults)
(require 'init-editing-utils)
(require 'init-gui-frames)
(require 'init-windows)

;; GTD/PKM
(require 'init-org)
(require 'init-denote)
(require 'init-elfeed)
(require 'init-wombag)

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

;; Tools
(require 'init-git)
(require 'init-pass)
(require 'init-mail)
(require 'init-irc)
(require 'init-gpt)
(require 'init-pdf)
(require 'init-lookup)
(require 'init-network-tools)
(require 'init-spell-checker)
