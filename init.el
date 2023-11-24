(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-elpa)
(require 'init-lib)

(setq custom-file (locate-user-emacs-file "custom.el"))
(setq native-comp-async-report-warnings-errors 'silent)

(require 'init-better-defaults)
(require 'init-macos-keybindings)
(require 'init-editing-utils)
(require 'init-gui-frames)
(require 'init-mode-line)
(require 'init-themes)
(require 'init-fonts)
(require 'init-minibuffer)
(require 'init-corfu)
(require 'init-denote)
(require 'init-org)
(require 'init-git)
(require 'init-github)
(require 'init-sourcehut)
(require 'init-nix)
(require 'init-ruby)
(require 'init-elixir)
(require 'init-web)
(require 'init-pass)
(require 'init-mail)
(require 'init-rss)
(require 'init-irc)
(require 'init-chatgpt)
(require 'init-mastodon)
(require 'init-upload)
(require 'init-lookup)
(require 'init-evil-keybindings)
(require 'init-tree-sitter)
(require 'init-workspaces)
(require 'init-popups)
(require 'init-server)
