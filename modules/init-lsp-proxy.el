;; brand new one testing

(use-package lsp-proxy
  :ensure (:host github
                 :repo "jadestrong/lsp-proxy"
                 :files ("lsp-proxy.el" "lsp-proxy")
                 )
  :config
  (add-hook 'tsx-ts-mode-hook #'lsp-proxy-mode)
  (add-hook 'js-ts-mode-hook #'lsp-proxy-mode)
  (add-hook 'typescript-mode-hook #'lsp-proxy-mode)
  (add-hook 'typescript-ts-mode-hook #'lsp-proxy-mode)
  )

(provide 'init-lsp-proxy)
