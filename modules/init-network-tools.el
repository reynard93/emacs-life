(use-package mastodon
  :pin nongnu
  :defer t
  :custom
  (mastodon-instance-url "https://hachyderm.io")
  (mastodon-active-user "goofansu"))

(use-package 0x0
  :pin melpa
  :bind (:map embark-region-map ("U" . 0x0-dwim)))

(provide 'init-network-tools)
