(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Bootstrap
(require 'init-bootstrap)
(require 'init-emacs)
(require 'init-pass)

;; Agenda/Note-taking
(require 'init-org)
(require 'init-denote)

;; Programming
(require 'init-tree-sitter)
(require 'init-eglot)
(require 'init-langs)

;; Reading
(require 'init-elfeed)
(require 'init-wombag)
(require 'init-epub)
(require 'init-pdf)

;; Tools
(require 'init-git)
(require 'init-gpt)
(require 'init-mail)
(require 'init-search)
(require 'init-social)
(require 'init-spell-checker)

;; Lisp
(require 'init-lisp)
