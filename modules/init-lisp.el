(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; built-in functions
(require 'lisp-buffer)
(require 'lisp-file)
(require 'lisp-org)

;; macOS
(require 'lisp-alfred)
(require 'lisp-macos)

;; Web services
(require 'lisp-gh)
(require 'lisp-exercism)

(provide 'init-lisp)
