(use-package elfeed
  :pin melpa
  :bind
  (("C-c e" . elfeed)
   :map elfeed-search-mode-map
   ("=" . +elfeed/summarize)
   ("B" . +elfeed/eww)
   ("D" . +elfeed/delete)
   ("S" . +elfeed/set-filter)
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

  (defun +elfeed/set-filter ()
    (interactive)
    (let ((categories
           '(("unread" . "@6-months-ago +inbox +unread")
             ("news" . "@6-months-ago +inbox +unread +news")
             ("engineering" . "@6-months-ago +inbox +unread +engineering")
             ("people" . "@6-months-ago +inbox +unread +people")
             ("emacs" . "@6-months-ago +inbox +unread +emacs"))))
      (if-let* ((category (completing-read "Select category: " categories))
                (filter (cdr (assoc category categories))))
          (elfeed-search-set-filter filter)
        (elfeed-search-set-filter category))))

  ;; Org export uses Elfeed entry's original link
  ;; https://takeonrules.com/2024/08/11/exporting-org-mode-elfeed-links/
  (org-link-set-parameters "elfeed"
                           :follow #'elfeed-link-open
                           :store #'elfeed-link-store-link
                           :export #'elfeed-link-export-link)

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

(use-package elfeed-org
  :pin melpa
  :after elfeed
  :config
  (elfeed-org))

(use-package elfeed-tube
  :pin melpa
  :after elfeed
  :bind
  ( :map elfeed-show-mode-map
    ([remap save-buffer] . elfeed-tube-save)
    ("F" . elfeed-tube-fetch)
    :map elfeed-search-mode-map
    ([remap save-buffer] . elfeed-tube-save)
    ("F" . elfeed-tube-fetch))
  :config
  (elfeed-tube-setup))

(use-package elfeed-tube-mpv
  :pin melpa
  :after elfeed
  :bind
  ( :map elfeed-show-mode-map
    ("C-c C-f" . elfeed-tube-mpv-follow-mode)
    ("C-c C-w" . elfeed-tube-mpv-where)))

(provide 'init-elfeed)
