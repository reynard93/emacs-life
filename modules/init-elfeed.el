(use-package elfeed
  :pin melpa
  :bind
  (("C-c e" . elfeed)
   :map elfeed-search-mode-map
   ("=" . +elfeed/summarize)
   ("B" . +elfeed/eww)
   ("D" . +elfeed/delete)
   ("R" . +elfeed/send-to-wombag)
   ("S" . +elfeed/set-filter)
   ("W" . +elfeed/switch-to-wombag)
   :map elfeed-show-mode-map
   ("=" . +elfeed/summarize)
   ("B" . +elfeed/eww)
   ("D" . +elfeed/delete)
   ("R" . +elfeed/send-to-wombag))

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

  (defun +elfeed/send-to-wombag (entry)
    (interactive (list (+elfeed--selected-entry)))
    (+wombag/url (elfeed-entry-link entry))
    (elfeed-tag entry 'sent)
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

  (defun +elfeed/switch-to-wombag ()
    (interactive)
    (if-let ((buf (get-buffer "*wallabag-search*")))
        (switch-to-buffer buf)
      (wombag)))

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
             ("videos" . "@6-months-ago +inbox +unread +video")
             ("podcasts" . "@6-months-ago +inbox +unread +podcast")
             ("emacs" . "@6-months-ago +inbox +unread +emacs"))))
      (if-let* ((category (completing-read "Select category: " categories))
                (filter (cdr (assoc category categories))))
          (elfeed-search-set-filter filter)
        (elfeed-search-set-filter category)))))

(use-package elfeed-org
  :pin melpa
  :after elfeed
  :custom
  (rmh-elfeed-org-files (list "elfeed.org"))
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
