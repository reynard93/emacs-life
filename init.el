(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; bootstrap
(setq custom-file (locate-user-emacs-file "custom.el"))
(require 'init-lib)
(require 'init-lisp)
(require 'init-elpa)
(require 'init-envrc)

;; interaction
(require 'init-minibuffer)
(require 'init-completion)
(require 'init-window)
(require 'init-evil)

;; visualization
(require 'init-ui)
(require 'init-pulse)
(require 'init-mode-line)

;; common
(require 'init-better-defaults)
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
(require 'init-irc)
(require 'init-gpt)
(require 'init-timer)
(require 'init-lookup)
(require 'init-reading)
(require 'init-restclient)
(require 'init-network-tools)

;; keybindings
(require 'init-macos-keybindings)
(require 'init-evil-keybindings)

;; customizations
(load custom-file t)

(provide 'init)
