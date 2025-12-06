;;; -*- lexical-binding: t -*-

(use-package elfeed
  :bind ("C-c e" . elfeed)
  :custom
  (elfeed-db-directory
   (expand-file-name "elfeed" user-emacs-directory))
  (elfeed-show-entry-switch 'display-buffer))

(use-package elfeed-org
  :config
  (elfeed-org)
  :custom
  (rmh-elfeed-org-files
   (list (concat (file-name-as-directory (getenv "HOME"))
		         "elfeed.org"))))
;; this elfeed.org file should be saved somewhere

(provide 'init-elfeed)
