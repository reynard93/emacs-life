(use-package gh
  :ensure nil
  :load-path "site-lisp/"
  :bind (:map embark-region-map ("G" . gh-gist-create)))

(provide 'init-github)
