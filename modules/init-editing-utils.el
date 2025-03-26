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

(use-package puni
  ;; works better in web-mode also more performant compared to smart parens
  :init
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode)
  )
; puni pairs with electric-mode with smartparens electric mode is not required
; handles auto-pairing, cons: single brac chars supported only, cant autoclose if...end or <p></p>
; although some major/minor modes come with their own tools for auto-pairing
(use-package electric
  :ensure nil
  :config
  (electric-indent-mode -1) ; dont randomly indent
  (electric-pair-mode))

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

;; Snippets
;; use tempel instead
;; (use-package yasnippet) ; this is known to add startup time significantly


;; Robe (note that for robe M. does robe-jump currently it is embark-dwim) need to bind to soemthing else
;; advise here is from wikimacs it is awesome
;; (require 'robe)
;; (add-hook 'ruby-mode-hook 'robe-mode)

(provide 'init-editing-utils)
