(use-package apheleia
  :pin melpa
  :bind ("C-c f" . apheleia-format-buffer))

(use-package evil-nerd-commenter
  :pin nongnu
  :bind ("s-;" . evilnc-comment-or-uncomment-lines))

(use-package multiple-cursors
  :pin nongnu
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(use-package smartparens
  :pin melpa
  :config
  (require 'smartparens-config)
  :hook (prog-mode text-mode conf-mode))

(use-package substitute
  :bind-keymap ("C-c s" . substitute-prefix-map))

(use-package wgrep
  :pin nongnu
  :after grep
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

(provide 'init-editing-utils)
