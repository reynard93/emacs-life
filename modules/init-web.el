(use-package web-mode
  :defer t
  :mode "\.erb\'"
  :mode "\.vue\'"
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

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
(use-package typescript-mode
  :commands (typescript-mode)
  :hook ((typescript-mode . rasen/setup-tide-mode)
         (typescript-mode . abbrev-mode))
  :init
  (el-patch-feature typescript-mode)
  (add-hook 'web-mode-hook
            (defun rasen/enable-typescript ()
              (when (member (file-name-extension buffer-file-name)
                            '("ts" "tsx" "js" "jsx"))
                (typescript-mode))))
  (add-hook 'rjsx-mode-hook #'rasen/enable-typescript)
  :config
  (setq-default typescript-indent-level 2)

  ;; Key bindings
  (define-key typescript-mode-map (kbd "M-j") #'c-indent-new-comment-line)
  (define-key typescript-mode-map (kbd "C-M-j") #'c-indent-new-comment-line)

  ;; Add more jsdoc tags? nope
  )


(provide 'init-web)
