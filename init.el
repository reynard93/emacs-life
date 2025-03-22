(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(defvar my-src-directory "~/src/"
  "Directory for source code repos.")

;; Performance optimizations should be loaded first
(require 'init-performance)

;; Bootstrap
(require 'init-bootstrap)
(require 'init-emacs)
(require 'init-macos)

;; Features
(require 'init-highlight)
(require 'init-elfeed)
(require 'init-git)
(require 'init-gpt)
(require 'init-mail)
(require 'init-pass)
(require 'init-search)
(require 'init-shell-command)
(require 'init-spell-checker)
(require 'init-syntax-checker)
; (require 'init-social)
(require 'init-http)

;; Contexts
(require 'init-programming)
(require 'init-howm)
(require 'init-writing)

;; Services
(require 'init-github)
(require 'init-reynard)
(require 'init-ews)
