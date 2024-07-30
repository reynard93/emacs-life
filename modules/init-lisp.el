(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; built-in functions
(require 'lisp-buffer)
(require 'lisp-file)
(require 'lisp-org)

;; macOS
(require 'lisp-alfred)
(require 'lisp-macos)

(provide 'init-lisp)
