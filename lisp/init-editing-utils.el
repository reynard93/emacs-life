(blink-cursor-mode -1)
(global-subword-mode 1)
(save-place-mode 1)

(setq-default indent-tabs-mode nil)

(use-package evil-nerd-commenter
  :pin nongnu
  :after evil
  :config
  (message "evil-nerd-commenter is loaded")
  (evil-define-key '(normal visual) 'global
    "gcc" #'evilnc-comment-or-uncomment-lines
    "gcp" #'evilnc-comment-or-uncomment-paragraphs
    "gcy" #'evilnc-copy-and-comment-lines))

(use-package evil-snipe
  :pin melpa
  :after evil
  :config
  (message "evil-snipe is loaded")
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

(use-package evil-surround
  :pin melpa
  :after evil
  :config
  (message "evil-surround is loaded")
  (global-evil-surround-mode 1))

(use-package evil-mc
  :pin melpa
  :after evil
  :config
  (message "evil-mc is loaded")
  (global-evil-mc-mode 1))

(use-package avy
  :after evil
  :config
  (message "avy is loaded")
  (evil-define-key '(normal visual) 'global
    "gss" #'evil-avy-goto-char-2)
  :custom
  (avy-all-windows nil))

(use-package anzu
  :pin melpa
  :defer 1
  :config
  (message "anzu is loaded")
  (global-anzu-mode 1))

(use-package evil-anzu
  :pin melpa
  :after (evil anzu)
  :config
  (message "evil-anzu is loaded"))

(use-package goggles
  :pin melpa
  :init
  (setq-default goggles-pulse t)
  :config
  (message "goggles is loaded")
  :hook (prog-mode text-mode))

(use-package rainbow-delimiters
  :pin nongnu
  :config
  (message "rainbow-delimiters is loaded")
  :hook prog-mode)

(use-package hl-todo
  :pin melpa
  :after evil
  :config
  (message "hl-todo is loaded")
  (evil-define-key 'motion 'global
    "]t" #'hl-todo-next
    "[t" #'hl-todo-previous)
  :hook prog-mode)

(use-package smartparens
  :pin melpa
  :config
  (message "smartparens is loaded")
  (require 'smartparens-config)
  :hook (prog-mode text-mode))

(use-package xclip
  :unless (display-graphic-p)
  :config
  (message "xclip is loaded")
  (xclip-mode 1))

(use-package ediff
  :ensure nil
  :defer t
  :config
  (message "ediff is loaded")
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally))

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
