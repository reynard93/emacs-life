(use-package org
  :ensure nil
  :init
  (setq org-directory "~/src/org/"
        org-agenda-files (list org-directory))

  :config
  (message "org is loaded")

  ;; Hooks
  (with-eval-after-load 'pulsar
    (dolist (hook '(org-agenda-after-show-hook org-follow-link-hook))
      (add-hook hook #'pulsar-recenter-center)
      (add-hook hook #'pulsar-reveal-entry)))

  ;; Advices
  (defun move-to-eol-advice (&rest args) (end-of-line))
  (advice-add 'org-meta-return :before #'move-to-eol-advice)
  (advice-add 'org-insert-todo-heading :before #'move-to-eol-advice)

  ;; Org babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (sql . t)
     (ruby . t)
     (shell . t)
     (racket . t)))

  :custom
  ;; General
  (org-use-sub-superscripts '{})
  (org-read-date-prefer-future 'time)
  (org-M-RET-may-split-line '((default . nil)))

  ;; Appearance
  (org-ellipsis "…")
  (org-hide-emphasis-markers t)

  ;; Editing
  (org-auto-align-tags nil)
  (org-tags-column 0)
  (org-catch-invisible-edits 'show-and-error)
  (org-special-ctrl-a/e t)
  (org-insert-heading-respect-content t)

  ;; Task
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-use-fast-todo-selection 'expert)
  (org-todo-keywords
   '((sequence "TODO(t)" "HOLD(h@/!)" "|" "DONE(d!)" "CANCELLED(c@)")))
  (org-todo-keyword-faces
   '(("HOLD" . (org-warning))
     ("CANCELLED" . (:strike-through t))))

  ;; Capture
  (org-capture-templates
   `(("u" "Unprocessed" entry
      (file+headline "tasks.org" "Unprocessed")
      ,(concat "* %^{Title}\n"
               ":PROPERTIES:\n"
               ":CAPTURED: %U\n"
               ":END:\n\n"
               "%a\n%i%?")
      :empty-lines-after 1)
     ("e" "Email note (unprocessed)" entry
      (file+headline "tasks.org" "Unprocessed")
      ,(concat "* TODO %:subject :mail:\n"
               ":PROPERTIES:\n"
               ":CAPTURED: %U\n"
               ":END:\n\n"
               "%a\n%i%?")
      :empty-lines-after 1)
     ("w" "Add to the wishlist (may do some day)" entry
      (file+headline "tasks.org" "Wishlist")
      ,(concat "* %^{Title}\n"
               ":PROPERTIES:\n"
               ":CAPTURED: %U\n"
               ":END:\n\n"
               "%?")
      :empty-lines-after 1)
     ("c" "Clock in and do immediately" entry
      (file+headline "tasks.org" "Clocked tasks")
      ,(concat "* TODO %^{Title}\n"
               ":PROPERTIES:\n"
               ":EFFORT: %^{Effort estimate in minutes|5|10|15|30|45|60|90|120}\n"
               ":END:\n\n"
               "%a\n")
      :prepend t
      :clock-in t
      :clock-keep t
      :immediate-finish t
      :empty-lines-after 1)
     ("t" "Time-sensitive task" entry
      (file+headline "tasks.org" "Tasks with a date")
      ,(concat "* TODO %^{Title} %^g\n"
               "%^{How time sensitive it is|SCHEDULED|DEADLINE}: %^t\n"
               ":PROPERTIES:\n"
               ":CAPTURED: %U\n"
               ":END:\n\n"
               "%?")
      :empty-lines-after 1)))

  (org-capture-templates-contexts
   '(("e" ((in-mode . "notmuch-search-mode")
           (in-mode . "notmuch-show-mode")
           (in-mode . "notmuch-tree-mode")))))

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
  (org-structure-template-alist
   '(("s" . "src")
     ("e" . "src emacs-lisp")
     ("n" . "src nix")
     ("r" . "src ruby")
     ("q" . "quote")
     ("x" . "example")
     ("X" . "export")))

  ;; Export
  (org-export-with-sub-superscripts nil)
  (org-export-with-section-numbers nil)
  (org-export-dispatch-use-expert-ui t)

  :bind (("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         ("C-c l" . org-store-link)
         :map org-mode-map
         ("C-u C-c C-l" . org-toggle-link-display)
         ("C-M-S-h" . org-babel-mark-block)
         ("C-c i" . org-cite-insert)
         ("M-g o" . consult-org-heading)))

(use-package ox-hugo
  :pin melpa
  :after ox
  :config
  (message "ox-hugo is loaded"))

(use-package org-pandoc-import
  :vc (org-pandoc-import :url "https://github.com/tecosaur/org-pandoc-import.git")
  :after org
  :config
  (message "org-pandoc-import is loaded"))

(use-package org-anki
  :pin melpa
  :after org
  :defer t
  :config
  (message "org-anki is loaded")
  :custom
  (org-anki-inherit-tags nil))

(provide 'init-org)
