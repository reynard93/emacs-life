(use-package elfeed
  :bind ("C-c e" . elfeed)
  :custom
  (elfeed-feeds
   '("https://sachachua.com/blog/category/emacs-news/feed/"
     "https://world.hey.com/this.week.in.rails/feed.atom")))

(provide 'init-elfeed)
