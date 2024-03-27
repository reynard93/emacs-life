(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'lisp-buffer)
(require 'lisp-file)
(require 'lisp-git)
(require 'lisp-macos)
(require 'lisp-mail)
(require 'lisp-project)
(require 'lisp-exercism)
(require 'lisp-org)

(provide 'init-lisp)
