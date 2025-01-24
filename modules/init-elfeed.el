(use-package elfeed
  :pin melpa
  :bind ("C-c e" . elfeed)
  :custom
  (elfeed-feeds
   '("https://sachachua.com/blog/category/emacs-news/feed/")))

(provide 'init-elfeed)
