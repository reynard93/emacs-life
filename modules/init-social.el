(use-package mastodon
  :pin nongnu
  :defer t
  :custom
  (mastodon-instance-url "https://hachyderm.io")
  (mastodon-active-user "goofansu"))

(use-package exercism
  :ensure nil
  :load-path "site-lisp/")

(provide 'init-social)
