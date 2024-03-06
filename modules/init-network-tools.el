(use-package mastodon
  :pin nongnu
  :defer t
  :config
  (message "mastodon is loaded")
  :custom
  (mastodon-instance-url "https://hachyderm.io")
  (mastodon-active-user "goofansu"))

(use-package 0x0
  :pin melpa
  :defer t
  :config
  (message "0x0 is loaded")
  :bind ( :map embark-region-map
          ("U" . 0x0-upload-text)
          :map embark-file-map
          ("U" . 0x0-upload-file)))

(provide 'init-network-tools)
