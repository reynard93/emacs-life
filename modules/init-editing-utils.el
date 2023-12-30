(blink-cursor-mode -1)
(global-subword-mode 1)
(save-place-mode 1)

(setq-default indent-tabs-mode nil)

(use-package avy
  :after evil
  :config
  (message "avy is loaded")
  (evil-define-key '(normal visual) 'global
    "gss" #'evil-avy-goto-char-2)
  :custom
  (avy-all-windows nil))

(use-package hl-todo
  :pin melpa
  :after evil
  :config
  (message "hl-todo is loaded")
  (evil-define-key 'motion 'global
    "]t" #'hl-todo-next
    "[t" #'hl-todo-previous)
  :hook (prog-mode text-mode))

(use-package rainbow-delimiters
  :pin nongnu
  :config
  (message "rainbow-delimiters is loaded")
  :hook (prog-mode text-mode))

(use-package smartparens
  :pin melpa
  :config
  (message "smartparens is loaded")
  (require 'smartparens-config)
  :hook (prog-mode text-mode))

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
  :config
  (message "logos is loaded")
  :custom
  (logos-outlines-are-pages t)
  :bind (([remap narrow-to-region] . logos-narrow-dwim)
         ([remap forward-page]     . logos-forward-page-dwim)
         ([remap backward-page]    . logos-backward-page-dwim)))

(use-package olivetti
  :pin melpa
  :after logos
  :config
  (message "olivetti is loaded")
  :custom
  (olivetti-body-width 80))

(provide 'init-editing-utils)
