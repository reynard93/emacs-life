(use-package racket-mode
  :pin nongnu
  :defer t
  :config
  (message "racket-mode is loaded"))

(use-package ob-racket
  :vc (ob-racket :url "https://github.com/hasu/emacs-ob-racket"
                 :rev :newest)
  :after racket-mode
  :config
  (message "ob-racket is loaded"))

(use-package sicp
  :pin melpa
  :defer t
  :config
  (message "sicp is loaded"))

(provide 'init-racket)
