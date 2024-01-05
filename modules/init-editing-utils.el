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
  :hook prog-mode)

(use-package smartparens
  :pin melpa
  :config
  (message "smartparens is loaded")
  (require 'smartparens-config)
  :hook (prog-mode text-mode))

(provide 'init-editing-utils)
