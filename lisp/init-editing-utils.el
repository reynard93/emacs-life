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

(use-package goggles
  :pin melpa
  :defer t
  :init
  (setq-default goggles-pulse t)
  :config
  (message "goggles is loaded")
  :hook ((prog-mode text-mode) . goggles-mode))

(provide 'init-editing-utils)
