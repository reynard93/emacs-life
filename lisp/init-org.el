(use-package org
  :init
  (setq org-directory "~/src/org")
  (setq org-agenda-files (list org-directory))
  :bind ("C-c n F" . yejun/browse-org))

(defun yejun/browse-org ()
  (interactive)
  (let ((project (project-current nil org-directory)))
    (project-find-file-in nil nil project)))

(use-package evil-org
  :pin melpa
  :after org
  :config
  (message "evil-org is loaded")
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  :hook (org-mode . (lambda () evil-org-mode)))

(provide 'init-org)
