(use-package nov
  :pin melpa
  :defer t
  :mode ("\\.epub\\'" . nov-mode)
  :config
  (message "nov is loaded"))

(provide 'init-epub)
