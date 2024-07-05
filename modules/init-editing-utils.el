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
  :bind ("C-c f f" . apheleia-format-buffer))

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
  :hook (prog-mode text-mode)
  :bind (("C-M-a"     . sp-beginning-of-sexp)
         ("C-M-e"     . sp-end-of-sexp)
         ("C-<down>"  . sp-down-sexp)
         ("C-<up>"    . sp-up-sexp)
         ("M-<down>"  . sp-backward-down-sexp)
         ("M-<up>"    . sp-backward-up-sexp)
         ("C-M-f"     . sp-forward-sexp)
         ("C-M-b"     . sp-backward-sexp)
         ("C-M-n"     . sp-next-sexp)
         ("C-M-p"     . sp-previous-sexp)
         ("C-S-b"     . sp-backward-symbol)
         ("C-S-f"     . sp-forward-symbol)
         ("C-<right>" . sp-forward-slurp-sexp)
         ("C-<left>"  . sp-backward-slurp-sexp)
         ("M-<right>" . sp-forward-barf-sexp)
         ("M-<left>"  . sp-backward-barf-sexp)
         ("C-M-k"     . sp-kill-sexp)
         ("C-k"       . sp-kill-hybrid-sexp)
         ("M-k"       . sp-backward-kill-sexp)))

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
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

(use-package multiple-cursors
  :pin nongnu
  :config
  (message "multiple-cursors is loaded")
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)))

(provide 'init-editing-utils)
