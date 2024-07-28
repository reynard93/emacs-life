(use-package mastodon
  :pin nongnu
  :defer t
  :custom
  (mastodon-instance-url "https://hachyderm.io")
  (mastodon-active-user "goofansu"))

(use-package 0x0
  :pin melpa
  :defer t
  :bind ( :map embark-region-map
          ("U" . 0x0-upload-text)
          :map embark-file-map
          ("U" . 0x0-upload-file)))

(provide 'init-network-tools)
