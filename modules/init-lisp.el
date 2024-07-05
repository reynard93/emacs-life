(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'lisp-alfred)
(require 'lisp-buffer)
(require 'lisp-exercism)
(require 'lisp-git)
(require 'lisp-kagi)
(require 'lisp-macos)
(require 'lisp-org)
(require 'lisp-project)

(provide 'init-lisp)
