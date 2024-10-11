(use-package hut
  :ensure nil
  :load-path "site-lisp/"
  :bind (:map embark-region-map ("P" . hut-paste-create)))

(provide 'init-sourcehut)
