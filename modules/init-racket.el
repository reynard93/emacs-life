(use-package racket-mode
  :pin nongnu
  :defer t
  :config
  (message "racket-mode is loaded"))

(use-package ob-racket
  :ensure nil
  :after racket-mode
  :load-path "lisp/emacs-ob-racket"
  :config
  (message "ob-racket is loaded"))

(use-package sicp
  :pin melpa
  :defer t
  :config
  (message "sicp is loaded"))

(provide 'init-racket)
