(use-package kagi
  :pin melpa
  :bind (:map embark-url-map ("s" . kagi-summarize-url))
  :custom
  (kagi-api-token (lambda () (auth-source-pass-get 'secret "api-key/kagi")))
  (kagi-summarizer-default-language "EN")
  (kagi-summarizer-engine "agnes")
  (kagi-summarizer-cache t))

(provide 'init-kagi)
