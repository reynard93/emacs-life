(use-package elfeed
  :pin melpa
  :defer t
  :init
  (defvar elfeed-browse-url-handlers
    '(("https:\\/\\/www\\.youtu\\.*be." . mpv-browse-url)
      ("." . eww-browse-url)))

  (defun mpv-browse-url (url &rest args)
    (message "Streaming in mpv: %s" url)
    (start-process "mpv" nil "mpv" url))

  :config
  (message "elfeed is loaded")

  (defun +elfeed--selected-entry ()
    (pcase major-mode
      ('elfeed-search-mode
       (elfeed-search-selected :single))
      ('elfeed-show-mode
       elfeed-show-entry)))

  (defun +elfeed/read-later (entry)
    (interactive (list (+elfeed--selected-entry)))
    (+wombag/url (elfeed-entry-link entry))
    (elfeed-tag entry 'later)
    (elfeed-search-update--force))

  (defun +elfeed/summarize (entry)
    (interactive (list (+elfeed--selected-entry)))
    (+gptel/kagi-summarize-url (elfeed-entry-link entry))
    (elfeed-tag entry 'summarized)
    (elfeed-search-update--force))

  (defun +elfeed/switch-to-wombag ()
    (interactive)
    (if-let ((buf (get-buffer "*wallabag-search*")))
        (switch-to-buffer buf)
      (wombag)))

  (defun +elfeed/browse ()
    (interactive)
    (let ((browse-url-handlers elfeed-browse-url-handlers))
      (pcase major-mode
        ('elfeed-search-mode (elfeed-search-browse-url))
        ('elfeed-show-mode (elfeed-show-visit)))))

  (defun +elfeed/set-filter ()
    (interactive)
    (let ((categories
           '(("Default" . "@6-months-ago -later -paper")
             ("News" . "@6-months-ago +news")
             ("Papers" . "@6-months-ago +paper")
             ("Later" . "@6-months-ago +later")
             ("Summarized" . "@6-months-ago +summarized"))))
      (if-let* ((category (completing-read "Select category: " categories))
                (filter (cdr (assoc category categories))))
          (elfeed-search-set-filter filter)
        (elfeed-search-set-filter category))))

  (when (featurep 'evil)
    (evil-define-key 'normal elfeed-search-mode-map
      "=" #'+elfeed/summarize
      "B" #'+elfeed/browse
      "R" #'+elfeed/read-later
      "S" #'+elfeed/set-filter
      "W" #'+elfeed/switch-to-wombag)

    (evil-define-key 'normal elfeed-show-mode-map
      "=" #'+elfeed/summarize
      "B" #'+elfeed/browse
      "R" #'+elfeed/read-later))

  :custom
  (elfeed-search-remain-on-entry t)
  (elfeed-search-filter "@6-months-ago -later -paper"))

(use-package elfeed-org
  :pin melpa
  :after elfeed
  :init
  (setq rmh-elfeed-org-files (list "elfeed.org"))
  :config
  (message "elfeed-org is loaded")
  (elfeed-org))

(provide 'init-elfeed)
