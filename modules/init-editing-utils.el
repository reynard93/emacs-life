(blink-cursor-mode -1)
(global-subword-mode 1)
(save-place-mode 1)

(setq-default indent-tabs-mode nil)

(use-package apheleia
  :pin melpa
  :defer t)

(use-package avy
  :config
  (message "avy is loaded")
  (with-eval-after-load 'evil
    (evil-define-key 'motion 'global
      "gss" #'evil-avy-goto-char-2)))

(use-package hl-todo
  :pin melpa
  :config
  (message "hl-todo is loaded")
  (with-eval-after-load 'evil
    (evil-define-key 'motion 'global
      "]t" #'hl-todo-next
      "[t" #'hl-todo-previous))
  :hook (prog-mode text-mode))

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

(provide 'init-editing-utils)
