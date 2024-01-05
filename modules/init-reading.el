(use-package elfeed
  :pin melpa
  :defer t
  :config
  (message "elfeed is loaded")

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

  (when (featurep 'evil)
    (evil-define-key 'normal elfeed-search-mode-map
      "R" #'+elfeed/post-to-wombag
      "W" #'+elfeed/switch-to-wombag)
    (evil-define-key 'normal elfeed-show-mode-map
      "R" #'+elfeed/post-to-wombag))

  :custom
  (elfeed-search-remain-on-entry t))

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

  (defun +wombag/switch-to-elfeed ()
    (interactive)
    (if-let ((buf (get-buffer "*elfeed-search*")))
        (switch-to-buffer buf)
      (elfeed)))

  (when (and
         (featurep 'evil)
         (featurep 'evil-collection))
    (evil-collection-set-readonly-bindings 'wombag-search-mode-map)
    (evil-collection-set-readonly-bindings 'wombag-show-mode-map)
    (evil-define-key 'normal wombag-search-mode-map
      "E"  #'+wombag/switch-to-elfeed
      (kbd "<return>") #'wombag-search-show-entry
      (kbd "S-<return>") 'wombag-search-browse-url
      "y"  #'wombag-search-copy
      "A"  #'wombag-search-archive-entry
      "gr" #'wombag-search-update--force
      "gR" #'wombag-sync))

  :custom
  (wombag-host "https://app.wallabag.it")
  (wombag-username "goofansu")
  (wombag-password (auth-source-pass-get 'secret "app.wallabag.it"))
  (wombag-client-id "23745_3qjblkrgo0qo4w4cwscg0g88wk4408wckw0gc8oskwg0cgkocw")
  (wombag-client-secret (auth-source-pass-get "client_secret" "app.wallabag.it"))

  :bind (:map embark-url-map
              ("R" . +wombag/url)))

(use-package logos
  :init
  (setq-default logos-hide-cursor nil
                logos-hide-mode-line t
                logos-hide-buffer-boundaries t
                logos-hide-fringe t
                logos-variable-pitch nil
                logos-buffer-read-only nil
                logos-scroll-lock nil
                logos-olivetti t)
  :config
  (message "logos is loaded")
  :custom
  (logos-outlines-are-pages t)
  :bind (([remap narrow-to-region] . logos-narrow-dwim)
         ([remap forward-page]     . logos-forward-page-dwim)
         ([remap backward-page]    . logos-backward-page-dwim)))

(use-package olivetti
  :pin melpa
  :after logos
  :config
  (message "olivetti is loaded")
  :custom
  (olivetti-body-width 80))

(provide 'init-reading)
