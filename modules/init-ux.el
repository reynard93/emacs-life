(use-package whitespace
  :ensure nil
  :bind
  (("<f6>" . whitespace-mode)
   ("C-c z" . whitespace-cleanup)))

(use-package display-line-numbers
  :ensure nil
  :bind ("<f7>" . display-line-numbers-mode)
  :custom
  (display-line-numbers-widen t))

(use-package visual-line-mode
  :ensure nil
  :hook text-mode
  :bind ("<f8>" . visual-line-mode))

(use-package olivetti
  :custom
  (olivetti-minimum-body-width 120)
  (olivetti-recall-visual-line-mode-entry-state t))

(use-package header-line
  :ensure nil
  :load-path "site-lisp/"
  :hook (prog-mode text-mode conf-mode))

(use-package lin
  :config
  (lin-global-mode 1))

(use-package pulsar
  :custom
  (pulsar-pulse-region-functions pulsar-pulse-region-common-functions)
  :config
  (pulsar-global-mode 1))

(use-package uniquify
  :defer t
  :custom
  (uniquify-buffer-name-style 'forward))
(provide 'init-ux)
