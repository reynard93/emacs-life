(use-package flycheck
  ;; :bind
  ;; ("C-c f" . flycheck-show-buffer-diagnostics) ;; dn think i using this keymap, show diagnostic also wh fn?
  :hook ((prog-mode ruby-ts-mode)
         . flycheck-mode)
  :config
  ;; move it to where i have flycheck
  ;; Enable ESLint with Flycheck when available
  (when (executable-find "eslint")
    (flycheck-add-next-checker 'javascript-eslint 'append))
  ;; Use ESLint from node_modules when available
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/.bin/eslint"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))

  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules))

(use-package flycheck-jest
  :after flycheck
  )

(use-package consult-flycheck
  :after (consult flycheck))

(provide 'init-syntax-checker)
