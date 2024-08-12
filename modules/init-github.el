(use-package gh
  :ensure nil
  :load-path "site-lisp/"
  :init
  (defvar-keymap embark-gh-pr-map
    "c" #'gh-pr-checkout
    "o" #'gh-pr-browse
    "v" #'gh-pr-view
    "y" #'gh-pr-link)
  (with-eval-after-load 'embark
    (add-to-list 'embark-keymap-alist '(github-pull-request . embark-gh-pr-map)))
  (with-eval-after-load 'marginalia
    (add-to-list 'marginalia-prompt-categories '("Select pull request" . github-pull-request)))
  :bind
  (("C-c g v" . gh-pr-view)
   ("C-c g c" . gh-pr-create)
   :map embark-region-map
   ("G" . gh-gist-create)))

(provide 'init-github)
