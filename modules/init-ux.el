(use-package whitespace
  :elpaca nil
  :bind
  (("<f6>" . whitespace-mode)
   ("C-c z" . whitespace-cleanup)))

(use-package display-line-numbers
  :elpaca nil
  :bind ("<f7>" . display-line-numbers-mode)
  :custom
  (display-line-numbers-widen t))

(use-package visual-line-mode
  :elpaca nil
  :hook text-mode
  :bind ("<f8>" . visual-line-mode))

(use-package logos
  :init
  (setq-default logos-hide-cursor nil
                logos-hide-mode-line t
                logos-hide-buffer-boundaries t
                logos-hide-fringe t
                logos-variable-pitch nil
                logos-buffer-read-only nil
                logos-scroll-lock nil
                logos-olivetti t)
  :hook (elfeed-show-mode . logos-focus-mode)
  :bind
  (([remap narrow-to-region] . logos-narrow-dwim)
   ([remap forward-page]     . logos-forward-page-dwim)
   ([remap backward-page]    . logos-backward-page-dwim)
   ("M-]" . logos-forward-page-dwim)
   ("M-[" . logos-backward-page-dwim)
   ("<f5>" . logos-focus-mode))
  :custom
  (logos-outlines-are-pages t))

(use-package olivetti
  :custom
  (olivetti-minimum-body-width 120)
  (olivetti-recall-visual-line-mode-entry-state t))

(use-package header-line
  :elpaca nil
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

(provide 'init-ux)
