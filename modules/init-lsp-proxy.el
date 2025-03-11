;; brand new one testing

(use-package lsp-proxy
  :ensure (:host github
                 :repo "jadestrong/lsp-proxy"
                 :files ("lsp-proxy.el" "lsp-proxy")
                 )
  :config
  (setq lsp-proxy-log-level 3)
  (add-hook 'tsx-ts-mode-hook #'lsp-proxy-mode)
  (add-hook 'js-ts-mode-hook #'lsp-proxy-mode)
  (add-hook 'typescript-mode-hook #'lsp-proxy-mode)
  (add-hook 'typescript-ts-mode-hook #'lsp-proxy-mode)
  (add-hook 'ruby-ts-mode-hook #'lsp-proxy-mode)
  )

(provide 'init-lsp-proxy)
