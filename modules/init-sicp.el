(use-package sicp
  :pin melpa
  :defer t)

;; Use racket for SICP exercises.
;; https://docs.racket-lang.org/sicp-manual/index.html
(use-package racket-mode
  :pin nongnu
  :defer t)

(use-package ob-racket
  :vc (ob-racket :url "https://github.com/hasu/emacs-ob-racket.git")
  :defer t)

(provide 'init-sicp)
