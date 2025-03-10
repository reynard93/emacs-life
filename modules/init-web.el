;; TODO  clean up these modes, esp tide
(use-package web-mode
  :defer t
  :mode "\\.erb\\'"
  :mode "\\.vue\\'"
  :mode "\\.html\\'"
  :mode "\\.jsx\\'"
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t))

;; use built-in typescript modes
(use-package typescript-ts-mode
  :ensure nil
  :mode "\\.ts\\'"
  :mode "\\.tsx\\'"
  :config
  ;; Ensure tsx files use tsx-ts-mode
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode)))

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

(use-package emmet-mode
  :hook (web-mode css-mode)
  :config
  (setq emmet-move-cursor-between-quotes t))

;; SCSS mode
(use-package scss-mode
  :mode "\\.scss\\'"
  :custom
  (scss-compile-at-save nil)
  (css-indent-offset 2))

;; move it to where i have flycheck
;; Enable ESLint with Flycheck when available
;; does the following line work in isolation without an ysrrounding block?
(when (executable-find "eslint")
  (flycheck-add-next-checker 'javascript-eslint 'append))

;; Add Node.js modules to path
(use-package add-node-modules-path
  :hook ((typescript-ts-mode . add-node-modules-path)
         (tsx-ts-mode . add-node-modules-path)
         (js-mode . add-node-modules-path)
         (web-mode . add-node-modules-path)))

;; Improved JSX/TSX editing
(use-package rjsx-mode
  :mode "\\.jsx\\'")

;; Improved HTML editing
(use-package tagedit
  :hook (web-mode . tagedit-mode))

;; Improved CSS color visualization
(use-package rainbow-mode
  :hook ((css-mode scss-mode web-mode) . rainbow-mode))

(provide 'init-web)
