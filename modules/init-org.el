(use-package org
  :ensure nil
  :init
  (setq org-directory "~/src/org"
        org-agenda-files (list org-directory))

  :config
  (message "org is loaded")

  (defun my-org-mode-hook ()
    (setq-local evil-auto-indent nil))

  ;; Advices
  (defun move-to-eol-advice (&rest args) (end-of-line))
  (advice-add 'org-meta-return :before #'move-to-eol-advice)
  (advice-add 'org-insert-todo-heading :before #'move-to-eol-advice)

  ;; Org babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (ruby . t)
     (shell . t)
     (restclient . t)
     (racket . t)))

  :custom
  ;; General
  (org-use-sub-superscripts '{})
  (org-read-date-prefer-future 'time)
  (org-M-RET-may-split-line '((default . nil)))

  ;; Appearance
  (org-ellipsis "▾")
  (org-hide-emphasis-markers nil)
  (org-cycle-separator-lines 0)

  ;; Task
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-use-fast-todo-selection 'expert)
  (org-todo-keywords
   '((sequence "TODO(t)" "HOLD(h@/!)" "|" "DONE(d!)" "KILL(k@)")))
  (org-todo-keyword-faces
   '(("KILL" . (:inherit (italic org-warning)))
     ("HOLD" . (:inherit (italic org-warning)))))

  ;; Capture
  (org-capture-templates
   '(("t" "Tasks" entry (file "todo.org") "* TODO %?\n%i" :prepend t)
     ("n" "Notes" entry (file "notes.org") "* %?\n%i" :prepend t)
     ("j" "Journal" entry (file+olp+datetree "journal.org") "* %U %?\n%i")))

  ;; Refile
  (org-outline-path-complete-in-steps nil)
  (org-refile-use-outline-path 'file)
  (org-refile-targets
   '((nil :maxlevel . 3)
     (org-agenda-files :maxlevel . 3)))

  ;; Code
  (org-confirm-babel-evaluate nil)
  (org-src-preserve-indentation t)
  (org-src-window-setup 'other-window)

  ;; Export
  (org-export-with-sub-superscripts nil)
  (org-export-with-section-numbers nil)

  :hook (org-mode . my-org-mode-hook)
  :bind (("C-c l" . org-store-link)
         :map org-mode-map
         ("C-M-S-h" . org-babel-mark-block)
         ("C-c i" . org-cite-insert)))

(use-package ox-hugo
  :pin melpa
  :after ox
  :config
  (message "ox-hugo is loaded")
  :custom
  (org-hugo-delete-trailing-ws nil)
  (org-hugo-auto-set-lastmod t))

(use-package oc
  :ensure nil
  :after citar
  :config
  (message "oc is loaded")
  :custom
  (org-cite-global-bibliography citar-bibliography)
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar))

(use-package citar
  :pin melpa
  :defer t
  :init
  (setq citar-bibliography '("~/src/notes/reference/reference.bib"))
  (setq citar-notes-paths '("~/src/notes/reference"))
  :config
  (message "citar is loaded"))

(use-package org-anki
  :pin melpa
  :after org
  :defer t
  :config
  (message "org-anki is loaded"))

(use-package org-pandoc-import
  :vc (org-pandoc-import :url "https://github.com/tecosaur/org-pandoc-import.git")
  :after org
  :config
  (message "org-pandoc-import is loaded"))

(use-package org-superstar
  :pin melpa
  :after org
  :defer t
  :config
  (message "org-superstar is loaded")
  :custom
  (org-superstar-leading-bullet ?\s)
  (org-superstar-remove-leading-stars nil)
  (org-superstar-headline-bullets-list '("◉" "◎" "●" "○" "◈" "◇")))

(provide 'init-org)
