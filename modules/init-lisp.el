(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; built-in functions
(require 'lisp-buffer)
(require 'lisp-file)
(require 'lisp-org)
(require 'lisp-project)

;; macOS
(require 'lisp-alfred)
(require 'lisp-macos)

;; Web services
(require 'lisp-exercism)
(require 'lisp-gh)
(require 'lisp-kagi)

(provide 'init-lisp)
