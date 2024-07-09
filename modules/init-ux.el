(use-package visual-line-mode
  :ensure nil
  :config
  (message "visual-line-mode is loaded")
  :bind ("<f5>" . visual-line-mode)
  :hook (org-mode markdown-mode gptel-mode))

(use-package whitespace
  :ensure nil
  :config
  (message "whitespace is loaded")
  :bind (("<f6>" . whitespace-mode)
         ("C-c z" . delete-trailing-whitespace)))

(use-package display-line-numbers
  :ensure nil
  :config
  (message "display-lines-numbers is loaded")
  (setq-default display-line-numbers-type t)
  (setq-default display-line-numbers-widen t)
  :custom
  (display-line-numbers-major-tick 0)
  (display-line-numbers-minor-tick 0)
  :bind ("<f7>" . display-line-numbers-mode))

(use-package spacious-padding
  :if (display-graphic-p)
  :demand t
  :config
  (message "spacious-padding is loaded")
  (spacious-padding-mode 1)
  :custom
  (spacious-padding-subtle-mode-line t)
  :bind ("<f8>" . spacious-padding-mode))

(use-package logos
  :if (display-graphic-p)
  :init
  (setq-default logos-hide-cursor nil
                logos-hide-mode-line t
                logos-hide-buffer-boundaries t
                logos-hide-fringe t
                logos-variable-pitch nil
                logos-buffer-read-only nil
                logos-scroll-lock nil
                logos-olivetti t)
  :config
  (message "logos is loaded")
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
  :defer t
  :config
  (message "olivetti is loaded")
  :custom
  (olivetti-body-width 0.7)
  (olivetti-minimum-body-width 80)
  (olivetti-recall-visual-line-mode-entry-state t))

(use-package outline
  :ensure nil
  :config
  (message "outline is loaded")
  :custom
  (outline-minor-mode-highlight nil)
  (outline-minor-mode-cycle t)
  (outline-minor-mode-use-buttons nil)
  (outline-minor-mode-use-margins nil)
  :bind
  ("<f10>" . outline-minor-mode))

(provide 'init-ux)
