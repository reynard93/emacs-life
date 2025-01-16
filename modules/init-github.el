(use-package gh
  :ensure nil
  :load-path "site-lisp/"
  :bind
  (("C-c g c" . gh-pr-create)
   ("C-c g v" . gh-pr-view)
   :map embark-region-map
   ("G" . gh-gist-create)))

(provide 'init-github)
