(use-package logos
  :init
  (setq-default logos-hide-cursor nil
                logos-hide-mode-line t
                logos-hide-buffer-boundaries t
                logos-hide-fringe t
                logos-variable-pitch t
                logos-buffer-read-only nil
                logos-scroll-lock nil
                logos-olivetti nil)
  :config
  (message "logos is loaded")
  :custom
  (logos-outlines-are-pages t)
  :hook
  (org-mode . logos-focus-mode)
  (gptel-mode . logos-focus-mode)
  :bind (([remap narrow-to-region] . logos-narrow-dwim)
         ([remap forward-page]     . logos-forward-page-dwim)
         ([remap backward-page]    . logos-backward-page-dwim)))

(use-package olivetti
  :pin melpa
  :defer t
  :config
  (message "olivetti is loaded")
  :custom
  (olivetti-body-width 110)
  :hook (org-mode gptel-mode))

(use-package visual-line-mode
  :ensure nil
  :hook (org-mode gptel-mode))

(provide 'init-writing)
