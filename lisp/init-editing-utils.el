(blink-cursor-mode -1)
(global-subword-mode 1)
(save-place-mode 1)

(setq-default indent-tabs-mode nil)

(use-package evil
  :pin melpa
  :config
  (message "evil is loaded")
  (evil-mode 1)
  :custom
  (evil-want-keybinding nil)            ; required by evil-collection
  (evil-undo-system 'undo-redo)         ; required by `evil-redo'
  (evil-want-fine-undo t)
  (evil-ex-substitute-global t)
  (evil-move-cursor-back nil)
  (evil-kill-on-visual-paste nil)
  (evil-symbol-word-search t)
  (evil-vsplit-window-right t)
  (evil-split-window-below t))

(use-package evil-collection
  :pin melpa
  :after evil
  :config
  (message "evil-collection is loaded")
  (evil-collection-init))

(use-package evil-nerd-commenter
  :pin nongnu
  :after evil
  :config
  (message "evil-nerd-commenter is loaded")
  (general-define-key :states '(normal visual)
                      "gcc" #'evilnc-comment-or-uncomment-lines
                      "gcp" #'evilnc-comment-or-uncomment-paragraphs
                      "gcy" #'evilnc-copy-and-comment-lines))

(use-package evil-snipe
  :pin melpa
  :after evil
  :config
  (message "evil-snipe is loaded")
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))

(use-package avy
  :after evil
  :config
  (message "avy is loaded")
  (general-define-key :states '(normal visual)
                      "gss" #'evil-avy-goto-char-2))

(use-package goggles
  :pin melpa
  :init
  (setq-default goggles-pulse t)
  :config
  (message "goggles is loaded")
  :hook ((prog-mode text-mode) . goggles-mode))

(use-package rainbow-delimiters
  :pin nongnu
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'init-editing-utils)
