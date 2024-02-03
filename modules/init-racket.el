(use-package racket-mode
  :pin nongnu
  :defer t)

(use-package ob-racket
  :ensure nil
  :after racket-mode
  :load-path "lisp/emacs-ob-racket")

(use-package sicp
  :pin melpa
  :defer t)

(provide 'init-racket)
