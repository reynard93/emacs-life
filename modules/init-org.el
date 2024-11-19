(use-package org
  :ensure nil
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link)
   :map org-mode-map
   ([remap mark-defun] . org-babel-mark-block)
   ("M-g o" . consult-org-heading))

  :custom
  (org-ellipsis "…")
  (org-use-sub-superscripts '{})
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  ;; Agenda
  (org-agenda-files `(,org-directory))
  (org-agenda-window-setup 'current-window)

  ;; Capture
  (org-capture-templates
   '(("c" "New clocked task" entry
      (file+headline "tasks.org" "Clocked tasks")
      "* TODO %^{Title}"
      :prepend t
      :clock-in t
      :clock-keep t
      :immediate-finish t
      :empty-lines-after 1)
     ("f" "New note (with Org)" item (file "notes.org") "- %?")
     ("n" "New note (with Denote)" plain
      (file denote-last-path)
      #'denote-org-capture
      :no-save t
      :immediate-finish nil
      :kill-buffer t
      :jump-to-captured t)))

  ;; Code block
  (org-edit-src-content-indentation 0)
  (org-edit-src-persistent-message nil)
  (org-src-preserve-indentation t)
  (org-src-window-setup 'current-window)

  ;; Export
  (org-export-with-sub-superscripts '{})

  ;; Log
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-log-redeadline 'time)
  (setq org-log-reschedule 'time)

  ;; Tag
  (org-auto-align-tags nil)
  (org-tags-column 0)

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (racket . t)
     (shell . t)
     (ruby . t))))

(use-package ox-hugo
  :pin melpa
  :after org)

(use-package ox-gfm
  :pin melpa
  :after org)

(provide 'init-org)
