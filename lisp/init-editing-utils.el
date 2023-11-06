(blink-cursor-mode -1)
(global-subword-mode 1)

(setq-default indent-tabs-mode nil)

(use-package evil
  :pin nongnu
  :config
  (message "evil is loaded")
  (evil-mode 1)
  :custom
  (evil-undo-system 'undo-redo)
  (evil-want-fine-undo t)
  (evil-want-keybinding nil))

(use-package evil-collection
  :pin melpa
  :after evil
  :config
  (message "evil-collection is loaded")
  (evil-collection-init))

(provide 'init-editing-utils)
