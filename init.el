(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(setq custom-file (locate-user-emacs-file "custom.el"))
(require 'init-lib)
(require 'init-elpa)
(require 'init-env)

;; core
(require 'init-minibuffer)
(require 'init-mode-line)
(require 'init-window)
(require 'init-theme)
(require 'init-evil)

;; basic
(require 'init-better-defaults)
(require 'init-macos-keybindings)
(require 'init-evil-keybindings)
(require 'init-editing-utils)
(require 'init-gui-frames)

;; note-taking
(require 'init-denote)
(require 'init-org)

;; programming
(require 'init-tree-sitter)
(require 'init-eglot)
(require 'init-data)
(require 'init-elixir)
(require 'init-markdown)
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

(provide 'init)
