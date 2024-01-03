(use-package elfeed
  :pin melpa
  :defer t
  :config
  (message "elfeed is loaded")
  (evil-set-initial-state 'elfeed-search-mode 'emacs)

  (defun +elfeed/post-to-wombag (entries)
    (interactive (list (pcase major-mode
                         ('elfeed-search-mode
                          (elfeed-search-selected))
                         ('elfeed-show-mode
                          (list elfeed-show-entry)))))
    (dolist (entry (ensure-list entries))
      (+wombag/url (elfeed-entry-link entry))))

  (defun +elfeed/switch-to-wombag ()
    (interactive)
    (if-let ((buf (get-buffer "*wallabag-search*")))
        (switch-to-buffer buf)
      (wombag)))

  :bind ( :map elfeed-search-mode-map
          ("R" . +elfeed/post-to-wombag)
          ("W" . +elfeed/switch-to-wombag)
          :map elfeed-show-mode-map
          ("R" . +elfeed/post-to-wombag)))

(use-package elfeed-org
  :pin melpa
  :after elfeed
  :init
  (setq rmh-elfeed-org-files (list "elfeed.org"))
  :config
  (message "elfeed-org is loaded")
  (elfeed-org))

(use-package wombag
  :load-path "lisp/wombag"
  :commands (wombag wombag-add-entry)
  :defer t
  :init
  (defsubst +wombag/url (url)
    "Add URL to Wombag."
    (wombag-add-entry url ""))

  :config
  (message "wombag is loaded")
  (evil-set-initial-state 'wombag-search-mode 'emacs)

  (defun +wombag/switch-to-elfeed ()
    (interactive)
    (if-let ((buf (get-buffer "*elfeed-search*")))
        (switch-to-buffer buf)
      (elfeed)))

  :custom
  (wombag-host "https://app.wallabag.it")
  (wombag-username "goofansu")
  (wombag-password (auth-source-pass-get 'secret "app.wallabag.it"))
  (wombag-client-id "23745_3qjblkrgo0qo4w4cwscg0g88wk4408wckw0gc8oskwg0cgkocw")
  (wombag-client-secret (auth-source-pass-get "client_secret" "app.wallabag.it"))

  :bind (:map embark-url-map
         ("R" . +wombag/url)))

(use-package wombag-search
  :ensure nil
  :after wombag
  :bind ( :map wombag-search-mode-map
          ("E" . +wombag/switch-to-elfeed)))

(provide 'init-reading)
