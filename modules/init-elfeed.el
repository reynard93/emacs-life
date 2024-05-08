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

  (defun +elfeed/browse ()
    (interactive)
    (let ((browse-url-handlers elfeed-browse-url-handlers))
      (pcase major-mode
        ('elfeed-search-mode (elfeed-search-browse-url))
        ('elfeed-show-mode (elfeed-show-visit)))))

  (when (featurep 'evil)
    (evil-define-key 'normal elfeed-search-mode-map "B" #'+elfeed/browse)
    (evil-define-key 'normal elfeed-show-mode-map "B" #'+elfeed/browse))

  :custom
  (elfeed-search-remain-on-entry t))

(use-package elfeed-org
  :pin melpa
  :after elfeed
  :config
  (message "elfeed-org is loaded")
  (elfeed-org))

(provide 'init-elfeed)
