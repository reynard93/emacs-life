(use-package elfeed
  :bind ("C-c e" . elfeed)
  :custom
  (elfeed-feeds
   '("https://protesilaos.com/codelog.xml")))

(provide 'init-elfeed)
