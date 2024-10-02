(use-package gh
  :ensure nil
  :load-path "site-lisp/"
  :bind
  (("C-c g c" . gh-pr-create)
   :map embark-region-map
   ("G" . gh-gist-create)))

(use-package consult-gh
  :pin melpa
  :after consult
  :bind
  (("C-c g p" . consult-gh-pr-list)
   ("C-c g i" . consult-gh-issue-list))
  :custom
  (consult-gh-show-preview t)
  (consult-gh-preview-key "C-o"))

(use-package consult-gh-embark
  :pin melpa
  :after (consult-gh embark)
  :config
  (consult-gh-embark-mode 1))

(provide 'init-github)
