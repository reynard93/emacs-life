(use-package elfeed
  :pin melpa
  :init
  (run-at-time t (* 8 60 60) #'elfeed-update) ; Update feeds every 8 hours
  (setq elfeed-feeds
        '("https://ferd.ca/feed.rss"
          "https://jvns.ca/atom.xml"
          "https://sive.rs/en.atom"
          "https://world.hey.com/dhh/feed.atom"
          "https://world.hey.com/jason/feed.atom"
          "https://world.hey.com/jorge/feed.atom"
          "https://www.feltpresence.com/rss/"
          "https://drewdevault.com/blog/index.xml"
          "https://emersion.fr/blog/atom.xml"
          "https://martinfowler.com/feed.atom"
          "https://jesseduffield.com/feed.xml"
          "https://andrealeopardi.com/feed.xml"
          "https://underjord.io/feed.xml"
          "https://xenodium.com/rss.xml"
          ;; Newsletter
          ("https://sachachua.com/blog/category/emacs-news/feed" emacs newsletter)
          ("https://world.hey.com/this.week.in.rails/feed.atom" rails newsletter)
          ("https://postgresweekly.com/rss/" postgresql weekly newsletter)
          ("https://rubyweekly.com/rss/" ruby newsletter)
          ;; Topics
          ("https://irreal.org/blog/?feed=rss2" emacs)
          ("https://protesilaos.com/codelog.xml" emacs)
          ("https://karthinks.com/tags/emacs/index.xml" emacs)
          ("https://www.ruby-lang.org/en/feeds/news.rss" ruby)
          ("https://dev.37signals.com/feed/posts.xml" rails)
          ("https://railsatscale.com/feed.xml" rails)
          ("https://dashbit.co/feed" elixir)
          ("https://news.livebook.dev/rss.xml" elixir)
          ("https://elixirstatus.com/rss" elixir)
          ("https://fly.io/phoenix-files/feed.xml" elixir)
          ("https://devenv.sh/feed_rss_created.xml" nix)))

  :bind
  (("C-c e" . elfeed)
   :map elfeed-search-mode-map
   ("=" . +elfeed/summarize)
   ("B" . +elfeed/eww)
   ("D" . +elfeed/delete)
   :map elfeed-show-mode-map
   ("=" . +elfeed/summarize)
   ("B" . +elfeed/eww)
   ("D" . +elfeed/delete))

  :custom
  (elfeed-initial-tags '(unread inbox))
  (elfeed-search-filter "#50 +unread ")

  :config
  (defun +elfeed--selected-entry ()
    (pcase major-mode
      ('elfeed-search-mode
       (elfeed-search-selected :single))
      ('elfeed-show-mode
       elfeed-show-entry)))

  (defun +elfeed/delete (entry)
    (interactive (list (+elfeed--selected-entry)))
    (elfeed-untag entry 'inbox)
    (if (eq major-mode 'elfeed-search-mode)
        (elfeed-search-update--force)
      (elfeed-show-refresh)))

  (defun +elfeed/eww ()
    (interactive)
    (let ((browse-url-browser-function #'eww))
      (pcase major-mode
        ('elfeed-search-mode (elfeed-search-browse-url))
        ('elfeed-show-mode (elfeed-show-visit)))))

  ;; Org export uses Elfeed entry's original link
  ;; https://takeonrules.com/2024/08/11/exporting-org-mode-elfeed-links/
  (with-eval-after-load 'org
    (org-link-set-parameters "elfeed"
                             :follow #'elfeed-link-open
                             :store #'elfeed-link-store-link
                             :export #'elfeed-link-export-link))

  (defun elfeed-link-export-link (link desc format _protocol)
    "Export `org-mode' `elfeed' LINK with DESC for FORMAT."
    (if (string-match "\\([^#]+\\)#\\(.+\\)" link)
        (if-let* ((entry
                   (elfeed-db-get-entry
                    (cons (match-string 1 link)
                          (match-string 2 link))))
                  (url
                   (elfeed-entry-link entry))
                  (title
                   (elfeed-entry-title entry)))
            (pcase format
              ('html (format "<a href=\"%s\">%s</a>" url desc))
              ('md (format "[%s](%s)" desc url))
              ('latex (format "\\href{%s}{%s}" url desc))
              ('texinfo (format "@uref{%s,%s}" url desc))
              (_ (format "%s (%s)" desc url)))
          (format "%s (%s)" desc url))
      (format "%s (%s)" desc link))))

(provide 'init-elfeed)
