(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Bootstrap
(require 'init-bootstrap)
(require 'init-emacs)
(require 'init-macos)

;; Agenda/Note-taking
(require 'init-org)
(require 'init-denote)

;; Programming
(require 'init-tree-sitter)
(require 'init-eglot)
(require 'init-langs)

;; Reading
(require 'init-elfeed)
(require 'init-epub)
(require 'init-pdf)

;; Tools
(require 'init-git)
(require 'init-gpt)
(require 'init-irc)
(require 'init-mail)
(require 'init-pass)
(require 'init-search)
(require 'init-network-tools)
(require 'init-spell-checker)
