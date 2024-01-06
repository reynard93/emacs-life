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
  ;; undo
  (evil-undo-system 'undo-fu)           ; required by `evil-redo'
  (evil-want-fine-undo t)
  ;; copy
  (evil-visual-update-x-selection-p nil)
  ;; kill
  (evil-kill-on-visual-paste nil)
  ;; search
  (evil-symbol-word-search t)
  (evil-ex-visual-char-range t)
  ;; replace
  (evil-ex-substitute-global t)
  ;; move
  (evil-shift-width 2)
  (evil-move-cursor-back nil)
  (evil-move-beyond-eol t)
  ;; window
  (evil-want-C-w-in-emacs-state t)
  (evil-vsplit-window-right t)
  (evil-split-window-below t)
  :bind (("s-n" . evil-buffer-new)
         :map evil-ex-completion-map
         ("C-a" . evil-beginning-of-line)
         ("C-b" . evil-backward-char)
         ("C-f" . evil-forward-char)
         :map evil-ex-search-keymap
         ("C-a" . evil-beginning-of-line)
         ("C-b" . evil-backward-char)
         ("C-f" . evil-forward-char)))

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
  (evil-define-key 'normal 'global
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

(use-package evil-anzu
  :pin melpa
  :after (evil anzu)
  :config
  (message "evil-anzu is loaded"))

(use-package evil-org
  :pin melpa
  :after (evil org)
  :config
  (message "evil-org is loaded")
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  :hook org-mode)

(provide 'init-evil)
