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

;; ESLint integration
(use-package eslint-fix
  :commands eslint-fix)

;; Define the setup function before using it in hooks
(defun rasen/setup-tide-mode ()
  "Setup function for tide mode."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)

  ;; Format options
  (setq tide-format-options
        '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t
          :placeOpenBraceOnNewLineForFunctions nil
          :indentSize 2
          :tabSize 2))

  ;; Enable ESLint with Flycheck when available
  (when (executable-find "eslint")
    (flycheck-add-next-checker 'typescript-tide 'javascript-eslint 'append)))

;; Prettier integration
(use-package prettier-js
  :hook ((typescript-ts-mode . prettier-js-mode)
         (tsx-ts-mode . prettier-js-mode)
         (js-mode . prettier-js-mode)
         (web-mode . prettier-js-mode))
  :config
  (setq prettier-js-args '("--single-quote" "--trailing-comma" "all")))

(use-package tide
  :hook ((typescript-ts-mode . rasen/setup-tide-mode)
         (tsx-ts-mode . rasen/setup-tide-mode)
         (js-mode . rasen/setup-tide-mode)
         (web-mode . (lambda ()
                       (when (or (string-equal "tsx" (file-name-extension buffer-file-name))
                                 (string-equal "ts" (file-name-extension buffer-file-name))
                                 (string-equal "js" (file-name-extension buffer-file-name))
                                 (string-equal "jsx" (file-name-extension buffer-file-name)))
                         (rasen/setup-tide-mode)))))
  :config
  (define-key tide-mode-map (kbd "K") nil)

  ;; Tide key bindings
  (define-key tide-mode-map (kbd "C-c C-d") 'tide-documentation-at-point)
  (define-key tide-mode-map (kbd "C-c C-r") 'tide-rename-symbol)
  (define-key tide-mode-map (kbd "C-c C-f") 'tide-fix)
  (define-key tide-mode-map (kbd "C-c C-o") 'tide-organize-imports)
  (define-key tide-mode-map (kbd "C-c C-i") 'tide-format)
  (define-key tide-mode-map (kbd "C-c C-e") 'tide-error-at-point)
  (define-key tide-mode-map (kbd "C-c C-l") 'tide-references)
  (define-key tide-mode-map (kbd "C-c C-j") 'tide-jump-to-definition)
  (define-key tide-mode-map (kbd "C-c C-b") 'tide-jump-back))

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
