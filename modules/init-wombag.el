(use-package wombag
  :vc (wombag :url "https://github.com/karthink/wombag.git")
  :commands (wombag-add-entry)
  :defer t
  :init
  (defsubst +wombag/url (url)
    "Add URL to Wombag."
    (message "Sending to Wombag: %s" url)
    (wombag-add-entry url ""))

  :config
  (message "wombag is loaded")

  (defun +wombag/switch-to-elfeed ()
    (interactive)
    (if-let ((buf (get-buffer "*elfeed-search*")))
        (switch-to-buffer buf)
      (elfeed)))

  (defun +wombag/show-eww-open ()
    "Open Wombag entry in EWW."
    (interactive)
    (when-let ((url (alist-get 'url wombag-show-entry)))
      (eww-browse-url url)))

  (defun +wombag/show-browse-url ()
    "Open Wombag entry in original URL."
    (interactive)
    (when-let ((url (alist-get 'url wombag-show-entry)))
      (browse-url url)))

  (defun +wombag/show-browse-host ()
    "Open Wombag entry on `wombag-host'."
    (interactive)
    (when-let* ((id (alist-get 'id wombag-show-entry))
                (url (format "%s/view/%s" wombag-host id)))
      (browse-url url)))

  (defun +wombag/search-browse-host ()
    "Open Wombag entry on `wombag-host'."
    (interactive)
    (wombag-search--with-entry
     (when-let* ((id (map-elt entry 'id))
                 (url (format "%s/view/%s" wombag-host id)))
       (browse-url url))))

  (when (and
         (featurep 'evil)
         (featurep 'evil-collection))
    (evil-collection-set-readonly-bindings 'wombag-search-mode-map)
    (evil-define-key 'normal wombag-search-mode-map
      (kbd "<return>") #'wombag-search-show-entry
      (kbd "S-<return>") 'wombag-search-browse-url
      "A"  #'wombag-search-archive-entry
      "B"  #'wombag-search-eww-open
      "D"  #'wombag-search-delete-entry
      "E"  #'+wombag/switch-to-elfeed
      "F"  #'wombag-search-starred-entry
      "s"  #'wombag-search-live-filter
      "y"  #'wombag-search-copy
      "go" #'wombag-search-browse-url
      "gO" #'+wombag/search-browse-host
      "gr" #'wombag-search-update--force
      "gR" #'wombag-sync)

    (evil-collection-set-readonly-bindings 'wombag-show-mode-map)
    (evil-define-key 'normal wombag-show-mode-map
      "B"  #'+wombag/show-eww-open
      "go" #'+wombag/show-browse-url
      "gO" #'+wombag/show-browse-host))

  :custom
  (wombag-host "https://app.wallabag.it")
  (wombag-username "goofansu")
  (wombag-password (auth-source-pass-get 'secret "app.wallabag.it"))
  (wombag-client-id "23745_3qjblkrgo0qo4w4cwscg0g88wk4408wckw0gc8oskwg0cgkocw")
  (wombag-client-secret (auth-source-pass-get "client_secret" "app.wallabag.it"))

  :bind (:map embark-url-map
              ("R" . +wombag/url)))

(provide 'init-wombag)
