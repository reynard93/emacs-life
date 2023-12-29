(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-elpa)
(require 'init-env)
(require 'init-lib)

(setq custom-file (locate-user-emacs-file "custom.el"))

;; basic
(require 'init-better-defaults)
(require 'init-macos-keybindings)
(require 'init-evil-keybindings)
(require 'init-editing-utils)
(require 'init-gui-frames)
(require 'init-windows)
(require 'init-popups)
(require 'init-themes)
(require 'init-fonts)

;; core
(require 'init-minibuffer)
(require 'init-completion)
(require 'init-mode-line)
(require 'init-pulse)

;; note-taking
(require 'init-org)
(require 'init-denote)

;; programming
(require 'init-tree-sitter)
(require 'init-eglot)
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
(require 'init-timer)
(require 'init-lookup)
(require 'init-network-tools)

;; customizations
(load custom-file t)
