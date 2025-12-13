;;; -*- lexical-binding: t -*-

(use-package apheleia
  :config
  ;; Use npx to run project-local prettier (respects project .prettierrc and plugins).
  ;; This ensures prettier-plugin-tailwindcss works when installed in the project.
  (setf (alist-get 'prettier apheleia-formatters)
        '("npx" "prettier" "--stdin-filepath" filepath))

  ;; Map modes to formatters.
  ;; Tree-sitter modes (js-ts-mode, tsx-ts-mode, etc.) are the primary modes for JS/TS.
  (setq apheleia-mode-alist
        '((js-mode . prettier)
          (js-ts-mode . prettier)
          (typescript-mode . prettier)
          (typescript-ts-mode . prettier)
          (tsx-ts-mode . prettier)
          (web-mode . prettier)
          (css-mode . prettier)
          (css-ts-mode . prettier)
          (json-mode . prettier)
          (json-ts-mode . prettier)
          (html-mode . prettier)
          (ruby-mode . rubocop)
          (ruby-ts-mode . rubocop)))

  ;; Enable formatting-on-save globally for supported modes.
  (apheleia-global-mode +1)

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
  :ensure
  ;; works better in web-mode also more performant compared to smart parens
  :init
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode)
  (add-hook 'org-mode-hook #'puni-disable-puni-mode)
  :bind ((:map puni-mode-map
          ;; Navigation
          ([remap forward-sexp] . puni-forward-sexp)
          ([remap backward-sexp] . puni-backward-sexp)
          ([remap transpose-sexps] . puni-transpose)

          ;; Deletion
          ([remap kill-word] . puni-forward-kill-word)
          ([remap backward-kill-word] . puni-backward-kill-word)
          ([remap kill-line] . puni-kill-line)
          ("C-S-k" . puni-backward-kill-line)
          ("C-w" . puni-kill-active-region)

          ;; Wrapping
          ("M-(" . puni-wrap-round)
          ("M-[" . puni-wrap-square)
          ("M-{" . puni-wrap-curly)
          ("M-<" . puni-wrap-angle) ;; now use M-g g 0 to go to top of buffer

          ;; Sexp manipulation
          ("M-S" . puni-splice)
          ("M-R" . puni-raise)
          ("M-C" . puni-convolute)
          ("M-D" . puni-squeeze)

          ;; Expand region - similar to expand-region package
          ("C-=" . puni-expand-region)
          ("C-<kp-equal>" . puni-expand-region)
          ("C--" . puni-contract-region))

         ;; Force delete for emergencies
         (nil . (("C-c DEL" . puni-force-delete))))
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

(provide 'init-editing-utils)
