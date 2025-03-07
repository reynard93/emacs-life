(use-package web-mode
  :defer t
  :mode "\.erb\'"
  :mode "\.vue\'"
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

;; use inbuit mode
(use-package typescript-ts-mode
  :ensure nil
  :mode "\\.ts\\'"
  :mode "\\.tsx\\'")

(use-package css-mode
  :ensure nil
  :defer t
  :custom
  (css-indent-offset 2))

(use-package js-mode
  :ensure nil
  :defer t
  :custom
  (js-indent-level 2))

(use-package haml-mode
  :defer t)

(use-package coffee-mode
  :defer t
  :custom
  (coffee-tab-width 2))

(use-package emmet-mode
  :hook (web-mode heex-ts-mode))

;; typescript related stuff stolen from https://github.com/rasendubi/dotfiles?tab=readme-ov-file#typescript
;; don't use typecript-mode no longer supported, refer to https://github.com/emacs-typescript/typescript.el
;; Essentially all major development of typescript-mode has come to a halt. use inbuilt typescript-ts-mode
(use-package tide
  :commands (tide-setup
             tide-hl-identifier-mode
             tide-format-before-save)
  :hook
  (typescript-ts-mode . rasen/setup-tide-mode)
  :init
  (defun rasen/setup-tide-mode ()
    (interactive)
    (with-demoted-errors "tide-setup: %S"
      (tide-setup))
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
    )
  :config
  (define-key tide-mode-map (kbd "K") nil))

(use-package flycheck-jest
  :after flycheck)

(provide 'init-web)
