(use-package racket-mode
  :pin nongnu
  :defer t)

(use-package ob-racket
  :vc (ob-racket :url "https://github.com/hasu/emacs-ob-racket.git")
  :defer t)

(provide 'init-racket)
