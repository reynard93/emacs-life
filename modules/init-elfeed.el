(use-package elfeed
  :pin melpa
  :init
  (setq elfeed-feeds
        '("https://sive.rs/en.atom"
          "https://jvns.ca/atom.xml"
          "https://world.hey.com/dhh/feed.atom"
          "https://world.hey.com/jorge/feed.atom"
          "https://world.hey.com/jason/feed.atom"
          "https://www.feltpresence.com/rss/"
          ("https://sachachua.com/blog/category/emacs-news/feed" emacs weekly)
          ("https://www.ruby-lang.org/en/feeds/news.rss" ruby)
          ("https://railsatscale.com/feed.xml" rails)
          ("https://world.hey.com/this.week.in.rails/feed.atom" rails)
          ("https://elixir-lang.org/atom.xml" elixir)
          ("https://www.erlang.org/blog.xml" erlang)
          ("https://dashbit.co/feed" elixir)
          ("https://elixirstatus.com/rss" elixir news)
          ("https://fly.io/phoenix-files/feed.xml" elixir)
          ("https://dev.37signals.com/feed/posts.xml" rails)
          ("https://protesilaos.com/codelog.xml" emacs)
          ("https://karthinks.com/tags/emacs/index.xml" emacs)
          ("https://irreal.org/blog/?feed=rss2" emacs)
          ("https://andrealeopardi.com/feed.xml" elixir)
          ("https://underjord.io/feed.xml" elixir)))

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
  (elfeed-search-remain-on-entry t)
  (elfeed-search-filter "@6-months-ago +inbox +unread")

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

  (defun +elfeed/summarize (entry)
    (interactive (list (+elfeed--selected-entry)))
    (+gptel/kagi-summarize-url (elfeed-entry-link entry))
    (elfeed-tag entry 'summarized)
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
