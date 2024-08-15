(use-package org
  :ensure nil

  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link)
   ("C-c o" . org-clock-goto)
   :map org-mode-map
   ([remap mark-defun] . org-babel-mark-block)
   ("M-g o" . consult-org-heading))

  :custom
  (org-directory "~/src/org/")
  (org-agenda-files (list org-directory))

  ;; Display
  (org-ellipsis "…")
  (org-use-sub-superscripts '{})

  ;; Tagging
  (org-tags-column 0)
  (org-auto-align-tags nil)

  ;; Logging
  (org-log-done 'time)
  (org-log-into-drawer t)

  ;; Code block
  (org-src-preserve-indentation t)

  ;; Export
  (org-export-dispatch-use-expert-ui t)
  (org-export-with-sub-superscripts '{})

  ;; Todo
  (org-use-fast-todo-selection 'expert)
  (org-todo-keywords
   '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

  ;; Capture
  (org-capture-templates
   `(("j" "Journal" entry
      (file+olp+datetree "journal.org")
      ,(concat "* %^{Title} %^g\n"
               ":PROPERTIES:\n"
               ":CAPTURED: %U\n"
               ":END:")
      :immediate-finish t)
     ("c" "Clocked task" entry
      (file+olp+datetree "journal.org")
      ,(concat "* TODO %^{Title} %^g\n"
               ":PROPERTIES:\n"
               ":CAPTURED: %U\n"
               ":END:")
      :clock-in t
      :clock-keep t
      :immediate-finish t))))

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
  :after org
  :bind (:map org-mode-map ("C-c M-y" . org-download-yank))
  :custom
  (org-download-image-dir "./attachments")
  (org-download-display-inline-images nil))

(use-package org-pandoc-import
  :vc (org-pandoc-import :url "https://github.com/tecosaur/org-pandoc-import.git"))

(provide 'init-org)
