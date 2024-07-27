(use-package whitespace
  :ensure nil
  :bind (("<f6>" . whitespace-mode)
         ("C-c z" . delete-trailing-whitespace)))

(use-package display-line-numbers
  :ensure nil
  :config
  (setq-default display-line-numbers-type t)
  (setq-default display-line-numbers-widen t)
  :custom
  (display-line-numbers-major-tick 0)
  (display-line-numbers-minor-tick 0)
  :bind ("<f7>" . display-line-numbers-mode))

(use-package spacious-padding
  :init
  (spacious-padding-mode 1)
  :custom
  (spacious-padding-subtle-mode-line t)
  :bind ("<f8>" . spacious-padding-mode))

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
  :custom
  (logos-outlines-are-pages t)
  :bind (([remap narrow-to-region] . logos-narrow-dwim)
         ([remap forward-page]     . logos-forward-page-dwim)
         ([remap backward-page]    . logos-backward-page-dwim)
         ("M-]" . logos-forward-page-dwim)
         ("M-[" . logos-backward-page-dwim)
         ("<f9>" . logos-focus-mode)))

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
  :config
  (setq-default goggles-pulse t)
  :hook (prog-mode text-mode conf-mode))

(use-package rainbow-delimiters
  :pin nongnu
  :hook (prog-mode text-mode conf-mode))

(provide 'init-ux)
