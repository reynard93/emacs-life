(blink-cursor-mode -1)
(global-subword-mode 1)
(save-place-mode 1)

(setq-default indent-tabs-mode nil)

(use-package avy
  :config
  (message "avy is loaded")
  (with-eval-after-load 'evil
    (evil-define-key 'motion 'global
      "gss" #'evil-avy-goto-char-2)))

(use-package hl-todo
  :pin melpa
  :config
  (message "hl-todo is loaded")
  (with-eval-after-load 'evil
    (evil-define-key 'motion 'global
      "]t" #'hl-todo-next
      "[t" #'hl-todo-previous))
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
