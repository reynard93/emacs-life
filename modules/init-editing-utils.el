(use-package apheleia
  :pin melpa
  :bind ("C-c f" . apheleia-format-buffer))

(use-package avy
  :bind ( :map goto-map
          ("s" . avy-goto-char-2)))

(use-package rainbow-delimiters
  :pin nongnu
  :hook prog-mode)

(use-package smartparens
  :pin melpa
  :config
  (require 'smartparens-config)
  :hook (prog-mode text-mode))

(use-package wgrep
  :pin nongnu
  :defer t
  :custom
  (wgrep-auto-save-buffer t))

(use-package substitute
  :bind-keymap ("C-c s" . substitute-prefix-map))

(use-package evil-nerd-commenter
  :pin nongnu
  :bind ("s-;" . evilnc-comment-or-uncomment-lines))

(use-package multiple-cursors
  :pin nongnu
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package move-text
  :pin melpa
  :bind (("s-<up>" . move-text-up)
         ("s-<down>" . move-text-down)))

(provide 'init-editing-utils)
