(blink-cursor-mode -1)
(global-subword-mode 1)

(setq-default indent-tabs-mode nil)

(use-package evil
  :pin nongnu
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
  (evil-vsplit-window-right t)
  (evil-split-window-below t))

(use-package evil-collection
  :pin melpa
  :after evil
  :config
  (message "evil-collection is loaded")
  (evil-collection-init))

(provide 'init-editing-utils)
