(use-package mastodon
  :pin nongnu
  :defer t
  :config
  (message "mastodon is loaded")
  :custom
  (mastodon-instance-url "https://hachyderm.io")
  (mastodon-active-user "goofansu"))

(provide 'init-mastodon)
