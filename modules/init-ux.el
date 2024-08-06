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
  :bind
  (([remap narrow-to-region] . logos-narrow-dwim)
   ([remap forward-page]     . logos-forward-page-dwim)
   ([remap backward-page]    . logos-backward-page-dwim)
   ("M-]" . logos-forward-page-dwim)
   ("M-[" . logos-backward-page-dwim)
   ("<f9>" . logos-focus-mode))
  :custom
  (logos-outlines-are-pages t))

(use-package olivetti
  :pin melpa
  :custom
  (olivetti-body-width 0.7)
  (olivetti-minimum-body-width 80)
  (olivetti-recall-visual-line-mode-entry-state t))

(use-package pulsar
  :config
  (pulsar-global-mode 1))

(use-package goggles
  :pin melpa
  :hook (prog-mode text-mode conf-mode)
  :config
  (setq-default goggles-pulse t))

(use-package rainbow-delimiters
  :pin nongnu
  :hook prog-mode)

(use-package header-line
  :ensure nil
  :load-path "site-lisp/"
  :hook (prog-mode text-mode conf-mode)
  :bind ("<f10>" . header-line-mode))

(provide 'init-ux)
