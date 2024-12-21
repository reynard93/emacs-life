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

  ;; Capture
  (org-capture-templates
   `(("t" "Task to do" entry
      (file+headline "tasks.org" "All tasks")
      ,(concat "* TODO %^{Title} %^g\n"
               ":PROPERTIES:\n"
               ":CAPTURED: %U\n"
               ":CUSTOM_ID: h:%(format-time-string \"%Y%m%dT%H%M%S\")\n"
               ":END:\n\n"
               "%a\n%?")
      :empty-lines-after 1)
     ("s" "Select file and heading to add to" entry
      (function prot-org-select-heading-in-file)
      ,(concat "* TODO %^{Title}%?\n"
               ":PROPERTIES:\n"
               ":CAPTURED: %U\n"
               ":CUSTOM_ID: h:%(format-time-string \"%Y%m%dT%H%M%S\")\n"
               ":END:\n\n")
      :empty-lines-after 1)
     ("f" "Fleeting note" plain
      (file denote-last-path)
      (function
       (lambda ()
         (let ((denote-use-keywords '("fleeting")))
           (denote-org-capture))))
      :no-save nil
      :immediate-finish nil
      :kill-buffer t
      :jump-to-captured nil)
     ("o" "OA ticket" plain
      (file denote-last-path)
      (function
       (lambda ()
         (let ((denote-use-title (alfred-browser-title))
               (denote-use-keywords '("jira" "openapply")))
           (denote-org-capture))))
      :no-save nil
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
  (require 'prot-org)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (racket . t)
     (shell . t)
     (ruby . t)
     (python . t))))

(provide 'init-org)
