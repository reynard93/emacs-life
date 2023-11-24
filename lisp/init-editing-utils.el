(blink-cursor-mode -1)
(global-subword-mode 1)
(save-place-mode 1)

(setq-default indent-tabs-mode nil)

(use-package evil
  :pin melpa
  :demand t
  :init
  (setq evil-normal-state-cursor 'box
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow)
  :config
  (message "evil is loaded")
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-mode 1)
  :custom
  (evil-want-keybinding nil)            ; required by evil-collection
  (evil-kill-on-visual-paste nil)       ; avoid adding replaced text to kill-ring
  ;; undo
  (evil-undo-system 'undo-redo)         ; required by `evil-redo'
  (evil-want-fine-undo t)
  ;; copy
  (evil-visual-update-x-selection-p nil)
  ;; search
  (evil-symbol-word-search t)
  (evil-ex-visual-char-range t)
  ;; replace
  (evil-ex-substitute-global t)
  ;; move
  (evil-move-cursor-back nil)
  (evil-move-beyond-eol nil)
  ;; window
  (evil-vsplit-window-right t)
  (evil-split-window-below t)
  :bind ( :map evil-ex-completion-map
          ("C-a" . evil-beginning-of-line)
          ("C-b" . evil-backward-char)
          ("C-f" . evil-forward-char)
          :map evil-ex-search-keymap
          ("C-a" . evil-beginning-of-line)
          ("C-b" . evil-backward-char)
          ("C-f" . evil-forward-char)))

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

(provide 'init-editing-utils)
