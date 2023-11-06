(use-package org
  :init
  (setq org-directory "~/src/org")
  (setq org-agenda-files (list org-directory))
  :config
  (message "org is loaded")
  (setq org-todo-keywords
        '((sequence "TODO(t)" "HOLD(h@/!)" "|" "DONE(d!)" "KILL(k@)")))
  (setq org-todo-keyword-faces
        '(("KILL" . (:inherit (italic org-warning)))
          ("HOLD" . (:inherit (italic org-warning)))))
  (setq org-capture-templates
        '(("t" "Tasks" entry (file "todo.org") "* TODO %?\n%i" :prepend t)
          ("j" "Journal" entry (file+olp+datetree "journal.org") "* %U %?\n%i")))
  :custom
  (org-startup-indented t)
  (org-src-preserve-indentation t)
  (org-confirm-babel-evaluate nil)
  (org-src-window-setup 'other-window)
  (org-log-into-drawer t)
  (org-log-done 'time)
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

(use-package ox-hugo
  :pin melpa
  :after ox
  :config
  (message "ox-hugo is loaded")
  :custom
  (org-hugo-delete-trailing-ws nil))

(use-package citar
  :pin melpa
  :init
  (setq citar-bibliography '("~/src/notes/reference.bib"))
  (setq citar-notes-paths '("~/src/notes/reference"))
  :config
  (message "citar is loaded")
  :bind (("C-c n b" . citar-open)
         ("C-c n B" . citar-open-notes)))

(use-package org-cite
  :ensure nil
  :after org
  :custom
  (org-cite-global-bibliography citar-bibliography)
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :bind (:map org-mode-map
         ("C-c i" . org-cite-insert)))

(push '("*Org Select*"
        (display-buffer-in-direction)
        (direction . below)
        (window-height . 0.3))
      display-buffer-alist)

(provide 'init-org)
