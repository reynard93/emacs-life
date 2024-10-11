(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Bootstrap
(require 'init-bootstrap)
(require 'init-emacs)
(require 'init-macos)

;; Features
(require 'init-browser)
(require 'init-dict)
(require 'init-git)
(require 'init-gpt)
(require 'init-irc)
(require 'init-mail)
(require 'init-org)
(require 'init-pass)
(require 'init-shell-command)
(require 'init-spell-checker)

;; Contexts
(require 'init-programming)
(require 'init-reading)
(require 'init-writing)

;; Services
(require 'init-devdocs)
(require 'init-github)
(require 'init-mastodon)
(require 'init-sourcehut)
