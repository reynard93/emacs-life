(use-package wombag
  :vc (wombag :url "https://github.com/karthink/wombag.git")
  :commands (wombag-add-entry)
  :defer t
  :init
  (defsubst +wombag/url (url)
    "Add URL to Wombag."
    (message "Sending to Wombag: %s" url)
    (wombag-add-entry url ""))
  :config
  (message "wombag is loaded")
  :custom
  (wombag-host "https://app.wallabag.it")
  (wombag-username "goofansu")
  (wombag-password (auth-source-pass-get 'secret "app.wallabag.it"))
  (wombag-client-id "23745_3qjblkrgo0qo4w4cwscg0g88wk4408wckw0gc8oskwg0cgkocw")
  (wombag-client-secret (auth-source-pass-get 'secret "app.wallabag.it/api-key"))
  :bind ( :map embark-url-map
          ("R" . +wombag/url)))

(provide 'init-wombag)
