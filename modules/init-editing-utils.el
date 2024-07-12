(blink-cursor-mode -1)
(global-subword-mode 1)
(save-place-mode 1)

(setq-default indent-tabs-mode nil
              fill-column 80)

(use-package apheleia
  :pin melpa
  :defer t
  :config
  (message "apheleia is loaded")
  :bind ("C-c f" . apheleia-format-buffer))

(use-package avy
  :config
  (message "avy is loaded")
  :bind ( :map goto-map
          ("s" . avy-goto-char-2)))

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

(use-package wgrep
  :pin nongnu
  :defer t
  :config
  (message "wgrep is loaded"))

(use-package substitute
  :config
  (message "substitute is loaded")
  :bind-keymap ("C-c s" . substitute-prefix-map))

(use-package evil-nerd-commenter
  :pin nongnu
  :defer t
  :config
  (message "evil-nerd-commenter is loaded")
  :bind ("s-;" . evilnc-comment-or-uncomment-lines))

(use-package multiple-cursors
  :pin nongnu
  :config
  (message "multiple-cursors is loaded")
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package move-text
  :pin melpa
  :config
  (message "move-text is loaded")
  :bind (("s-<up>" . move-text-up)
         ("s-<down>" . move-text-down)))

(provide 'init-editing-utils)
