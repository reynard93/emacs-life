(use-package racket-mode
  :defer t)

(use-package ob-racket
  :ensure (ob-racket :url "https://github.com/hasu/emacs-ob-racket.git")
  :after org
  :custom
  (ob-racket-default-lang "sicp"))

(use-package sicp
  :defer t)

(provide 'init-racket)
