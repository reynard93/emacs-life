(use-package apheleia
  :config
  (add-to-list 'apheleia-formatters
               '(prettier . ("prettier" "--stdin-filepath" filepath "--single-quote" "--trailing-comma" "all")))
  (setq apheleia-mode-alist
        '((js-mode . prettier)
          (typescript-ts-mode . prettier)
          (ruby-ts-mode . rubocop)
          (typescript-mode . prettier)
          (typescript-tsx-mode . prettier)
          (web-mode . prettier)))
  :bind ("C-c f" . apheleia-format-buffer))

(use-package multiple-cursors
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)))

(use-package visual-replace
  :defer t
  :bind (("C-c w r" . visual-replace)
         :map isearch-mode-map
         ("C-c w r" . visual-replace-from-isearch)))

;; (use-package rainbow-mode
;;   :hook (mhtml-mode))

(use-package smartparens
  :hook (prog-mode text-mode)
  :bind
  (:map smartparens-mode-map
   ("C-M-a" . sp-beginning-of-sexp)
   ("C-M-e" . sp-end-of-sexp)
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ("C-M-d" . sp-down-sexp)
   ("C-M-u" . sp-backward-up-sexp)
   ("C-M-w" . sp-copy-sexp)
   ("C-M-k" . sp-kill-sexp)
   ("C-M-<backspace>" . sp-backward-kill-sexp)) ; del whole word at cursor
  :config
  (require 'smartparens-config))

(use-package substitute
  :bind-keymap ("C-c s" . substitute-prefix-map))

(use-package wgrep
  :after grep
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

(use-package ws-butler
  :hook (prog-mode text-mode conf-mode)
  :custom
  (ws-butler-keep-whitespace-before-point nil))

;; this is so good, i missed this from neovim
(use-package expand-region
  :ensure t
  :bind
  (("C-=" . er/expand-region)
   ("C-=" . er/expand-region)
   ("C-<kp-equal>" . er/expand-region)
   ("C--" . er/contract-region))
  )

;; Snippets
;; use tempel instead
;; (use-package yasnippet) ; this is known to add startup time significantly

;; maybe KISS really SIMPLE? you might not even need all these

;; Robe (note that for robe M. does robe-jump currently it is embark-dwim) need to bind to soemthing else
;; advise here is from wikimacs it is awesome
;; (require 'robe)
;; (add-hook 'ruby-mode-hook 'robe-mode)
;; If you are using rvm you may want to instruct robe to auto-trigger rvm-activate-corresponding-ruby:
;; (defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
;;   (rvm-activate-corresponding-ruby))

(provide 'init-editing-utils)
