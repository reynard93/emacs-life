(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-elpa)
(require 'init-lib)

(setq custom-file (locate-user-emacs-file "custom.el"))
(setq native-comp-async-report-warnings-errors 'silent)

;; basic
(require 'init-better-defaults)
(require 'init-macos-keybindings)
(require 'init-evil-keybindings)
(require 'init-editing-utils)
(require 'init-gui-frames)
(require 'init-windows)

;; core
(require 'init-minibuffer)
(require 'init-completion)
(require 'init-appearance)
(require 'init-tree-sitter)
(require 'init-eglot)

;; note-taking
(require 'init-org)
(require 'init-denote)

;; programming
(require 'init-data)
(require 'init-elixir)
(require 'init-nix)
(require 'init-ruby)
(require 'init-web)
(require 'init-yaml)

;; tools
(require 'init-git)
(require 'init-pass)
(require 'init-mail)
(require 'init-rss)
(require 'init-irc)
(require 'init-lookup)
(require 'init-network-tools)

;; customizations
(load custom-file t)
