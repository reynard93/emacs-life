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
  (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  ;; Agenda
  (org-agenda-files `(,org-directory))
  (org-agenda-window-setup 'current-window)

  ;; Capture
  (org-capture-templates
   `(("t" "Task" entry
      (file "tasks.org")
      ,(concat "* TODO %^{Title}\n"
               ":PROPERTIES:\n"
               ":CAPTURED: %U\n"
               ":END:\n\n"
               "%a\n")
      :prepend t
      :immediate-finish t
      :empty-lines-after 1)))

  ;; Code block
  (org-edit-src-content-indentation 0)
  (org-edit-src-persistent-message nil)
  (org-src-preserve-indentation t)
  (org-src-window-setup 'current-window)

  ;; Log
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-log-redeadline 'time)
  (org-log-reschedule 'time)

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
