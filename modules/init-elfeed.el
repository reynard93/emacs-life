(use-package elfeed
  :pin melpa
  :init
  (setq elfeed-feeds
        '(("https://world.hey.com/dhh/feed.atom" webdev)
          ("https://world.hey.com/jorge/feed.atom" webdev)
          ("https://protesilaos.com/codelog.xml" emacs)
          ("https://karthinks.com/tags/emacs/index.xml" emacs)
          ("https://simonwillison.net/tags/llms.atom" llm)))

  :bind
  (("C-c e" . elfeed)
   :map elfeed-search-mode-map
   ("B" . +elfeed/eww)
   :map elfeed-show-mode-map
   ("B" . +elfeed/eww))

  :custom
  (elfeed-search-filter "#50 +unread ")

  :config
  (defun +elfeed--selected-entry ()
    (pcase major-mode
      ('elfeed-search-mode
       (elfeed-search-selected :single))
      ('elfeed-show-mode
       elfeed-show-entry)))

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
