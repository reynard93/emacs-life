(use-package elfeed
  :pin melpa
  :defer t
  :init
  (setq rmh-elfeed-org-files (list "elfeed.org"))

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

  (defun +elfeed/read-it-later (entry)
    (interactive (list (+elfeed--selected-entry)))
    (+wombag/url (elfeed-entry-link entry))
    (elfeed-tag entry 'later)
    (elfeed-untag entry 'unread)
    (elfeed-search-update--force))

  (defun +elfeed/summarize (entry)
    (interactive (list (+elfeed--selected-entry)))
    (kagi-summarize-url (elfeed-entry-link entry))
    (elfeed-tag entry 'summarized)
    (elfeed-untag entry 'unread)
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

  (when (featurep 'evil)
    (evil-define-key 'normal elfeed-search-mode-map
      "B" #'+elfeed/browse
      "K" #'+elfeed/summarize
      "L" #'+elfeed/read-it-later
      "W" #'+elfeed/switch-to-wombag)

    (evil-define-key 'normal elfeed-show-mode-map
      "B" #'+elfeed/browse
      "K" #'+elfeed/summarize
      "L" #'+elfeed/read-it-later))

  :custom
  (elfeed-search-remain-on-entry t)
  (elfeed-search-filter "@+12-months-ago -later -paper"))

(use-package elfeed-org
  :pin melpa
  :after elfeed
  :config
  (message "elfeed-org is loaded")
  (elfeed-org))

(provide 'init-elfeed)
