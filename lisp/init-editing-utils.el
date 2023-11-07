(blink-cursor-mode -1)
(global-subword-mode 1)
(save-place-mode 1)

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
  (evil-symbol-word-search t)
  (evil-vsplit-window-right t)
  (evil-split-window-below t))

(use-package evil-collection
  :pin melpa
  :after evil
  :config
  (message "evil-collection is loaded")
  (evil-collection-init))

(use-package goggles
  :pin melpa
  :defer t
  :init
  (setq-default goggles-pulse t)
  :config
  (message "goggles is loaded")
  :hook ((prog-mode text-mode) . goggles-mode))

(use-package cape
  :pin melpa
  :init
  (message "cape is loaded")
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  :bind (("C-c c p" . completion-at-point)
         ("C-c c t" . complete-tag)
         ("C-c c d" . cape-dabbrev)
         ("C-c c h" . cape-history)
         ("C-c c f" . cape-file)
         ("C-c c k" . cape-keyword)
         ("C-c c s" . cape-elisp-symbol)
         ("C-c c e" . cape-elisp-block)
         ("C-c c a" . cape-abbrev)
         ("C-c c l" . cape-line)
         ("C-c c w" . cape-dict)
         ("C-c c :" . cape-emoji)
         ("C-c c \\" . cape-tex)
         ("C-c c _" . cape-tex)
         ("C-c c ^" . cape-tex)
         ("C-c c &" . cape-sgml)
         ("C-c c r" . cape-rfc1345)))

(provide 'init-editing-utils)
