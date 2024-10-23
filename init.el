(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(defvar my-sync-directory "~/Library/CloudStorage/Dropbox/Emacs/"
  "Directory for Emacs sync files.")

;; Bootstrap
(require 'init-bootstrap)
(require 'init-emacs)
(require 'init-macos)

;; Features
(require 'init-alfred)
(require 'init-git)
(require 'init-gpt)
(require 'init-irc)
(require 'init-mail)
(require 'init-org)
(require 'init-pass)
(require 'init-search)
(require 'init-shell-command)
(require 'init-spell-checker)

;; Contexts
(require 'init-programming)
(require 'init-reading)
(require 'init-writing)

;; Services
(require 'init-github)
(require 'init-sourcehut)
