(use-package wombag
  :vc (wombag :url "https://github.com/karthink/wombag.git")
  :defer t
  :commands (wombag-add-entry)

  :init
  (with-eval-after-load 'embark
    (keymap-set embark-url-map "R" #'wombag-add-entry))

  :custom
  (wombag-host "https://app.wallabag.it")
  (wombag-username "goofansu")
  (wombag-password (auth-source-pass-get 'secret "app.wallabag.it"))
  (wombag-client-id "25398_849l75lzo6ww004w480w0kscc0gwssww04csgogscs8s0wskg")
  (wombag-client-secret (auth-source-pass-get 'secret "api-key/wallabag")))

(provide 'init-wombag)
