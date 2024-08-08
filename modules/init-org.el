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
  (org-directory "~/src/org/")
  (org-agenda-files '("tasks.org"))

  ;; Appearance
  (org-ellipsis "…")
  (org-tags-column 0)
  (org-auto-align-tags nil)

  ;; Capture
  (org-capture-templates
   `(("t" "General task" entry
      (file "tasks.org")
      "* TODO %?"
      :prepend t)
     ("s" "Scheduled task" entry
      (file "tasks.org")
      ,(concat "* TODO %^{Title} %^g\n"
               "%^{How time sensitive it is|SCHEDULED|DEADLINE}: %^t\n")
      :prepend t
      :immediate-finish t)
     ("c" "Clocked task" entry
      (file "tasks.org")
      ,(concat "* TODO %^{Title} %^g\n"
               ":PROPERTIES:\n"
               ":EFFORT: %^{Effort estimate in minutes|5|10|15|30|45|60|90|120}\n"
               ":END:\n\n")
      :prepend t
      :clock-in t
      :clock-keep t
      :immediate-finish t)
     ("n" "Note" entry
      (file "notes.org")
      "* %?")
     ("j" "Journal" entry
      (file+olp+datetree "journal.org")
      "* %?")))

  ;; Todo
  (org-use-fast-todo-selection 'expert)
  (org-todo-keywords '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

  ;; Logging
  (org-log-into-drawer t)
  (org-log-done t)

  ;; Code block
  (org-src-preserve-indentation t)

  ;; Export
  (org-export-dispatch-use-expert-ui t))

(use-package ox-hugo
  :pin melpa
  :after org)

(use-package org-anki
  :pin melpa
  :after org
  :custom
  (org-anki-default-deck "Default")
  (org-anki-default-match "@anki&todo<>\"TODO\"")
  (org-anki-inherit-tags nil))

(use-package org-download
  :pin melpa
  :bind (:map org-mode-map ("C-c M-y" . org-download-yank))
  :custom
  (org-download-image-dir "./attachments")
  (org-download-display-inline-images nil))

(use-package org-pandoc-import
  :vc (org-pandoc-import :url "https://github.com/tecosaur/org-pandoc-import.git"))

(provide 'init-org)
