(use-package gh
  :ensure nil
  :load-path "site-lisp/"
  :bind
  (("C-c g c" . gh-pr-create)
   :map embark-region-map
   ("G" . gh-gist-create)))

(provide 'init-github)
