(use-package elfeed
  :pin melpa
  :defer t
  :init
  (setq rmh-elfeed-org-files (list "data/elfeed.org"))

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

  (defun +elfeed/post-to-wombag (entry)
    (interactive (list (+elfeed--selected-entry)))
    (+wombag/url (elfeed-entry-link entry))
    (elfeed-untag entry 'unread)
    (elfeed-tag entry 'collected)
    (elfeed-search-update--force))

  (defun +elfeed/summarize (entry)
    (interactive (list (+elfeed--selected-entry)))
    (+kagi/summarize (elfeed-entry-link entry))
    (elfeed-untag entry 'unread)
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

  (when (featurep 'evil)
    (evil-define-key 'normal elfeed-search-mode-map
      "B" #'+elfeed/browse
      "K" #'+elfeed/summarize
      "R" #'+elfeed/post-to-wombag
      "W" #'+elfeed/switch-to-wombag)

    (evil-define-key 'normal elfeed-show-mode-map
      "B" #'+elfeed/browse
      "K" #'+elfeed/summarize
      "R" #'+elfeed/post-to-wombag))

  :custom
  (elfeed-search-remain-on-entry t))

(use-package elfeed-org
  :pin melpa
  :after elfeed
  :config
  (message "elfeed-org is loaded")
  (elfeed-org))

(provide 'init-elfeed)
