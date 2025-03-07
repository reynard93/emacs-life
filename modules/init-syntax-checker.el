(use-package flycheck
  :hook ruby-ts-mode
  :config
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
