(use-package elfeed
  :pin melpa
  :defer t
  :config
  (message "elfeed is loaded"))

(use-package elfeed-org
  :pin melpa
  :after elfeed
  :init
  (setq rmh-elfeed-org-files (list (expand-file-name "elfeed.org" org-directory)))
  :config
  (message "elfeed-org is loaded")
  (elfeed-org))

(provide 'init-rss)
