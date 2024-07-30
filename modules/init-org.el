(use-package org
  :ensure nil
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link)
   ("C-c o" . +org/toggle-last-clock)
   :map org-mode-map
   ("C-u C-c C-l" . org-toggle-link-display)
   ("C-c C-." . +org/refile-to-current-file)
   ("C-c C-'" . +org/refile-to-file)
   ("M-g o" . consult-org-heading))

  :custom
  (org-directory "~/src/org/")
  (org-agenda-files (list org-directory))
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
      :empty-lines-after 1)
     ("f" "Fleeting note" entry
      (file "notes.org")
      "* %?\n")
     ("j" "Journal" entry
      (file+olp+datetree "journal.org")
      "* %U %?\n")))

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
  (org-export-allow-bind-keywords t)

  :config
  (with-eval-after-load 'pulsar
    (dolist (hook '(org-agenda-after-show-hook org-follow-link-hook))
      (add-hook hook #'pulsar-recenter-center)
      (add-hook hook #'pulsar-reveal-entry)))

  ;; https://github.com/doomemacs/doomemacs/blob/4d072ce888577b023774460f6036abefcd0a1fa6/modules/lang/org/autoload/org-refile.el
  (defun +org/refile-to-current-file (arg &optional file)
    "Refile current heading to elsewhere in the current buffer.
If prefix ARG, copy instead of move."
    (interactive "P")
    (let ((org-refile-targets `((,file :maxlevel . 10)))
          (org-refile-use-outline-path t)
          (org-refile-keep arg)
          current-prefix-arg)
      (call-interactively #'org-refile)))

  (defun +org/refile-to-file (arg file)
    "Refile current heading to a particular org file.
If prefix ARG, copy instead of move."
    (interactive
     (list current-prefix-arg
           (read-file-name "Select file to refile to: "
                           default-directory
                           (buffer-file-name (buffer-base-buffer))
                           t nil
                           (lambda (f) (string-match-p "\\.org$" f)))))
    (+org/refile-to-current-file arg file))

  ;; https://github.com/doomemacs/doomemacs/blob/986398504d09e585c7d1a8d73a6394024fe6f164/modules/lang/org/autoload/org.el#L318-L336
  (defun +org/toggle-last-clock (arg)
    "Toggles last clocked item. Clock out if an active clock is
running (or cancel it if prefix ARG is non-nil). If no clock is
active, then clock into the last item. See `org-clock-in-last' to
see how ARG affects this command."
    (interactive "P")
    (require 'org-clock)
    (cond ((org-clocking-p)
           (if arg
               (org-clock-cancel)
             (org-clock-out)))
          ((and (null org-clock-history)
                (or (org-on-heading-p)
                    (org-at-item-p))
                (y-or-n-p "No active clock. Clock in on current item?"))
           (org-clock-in))
          ((org-clock-in-last arg))))

  ;; https://emacs.ch/@sachac/111732515520285898
  (defun +org/mastodon-toot-subtree ()
    "Compose a buffer and include the current subtree."
    (interactive)
    (let ((text (org-export-as 'md t nil t)))
      (mastodon-toot)
      (insert text))))

(use-package ox-hugo
  :pin melpa
  :after org)

(use-package org-pandoc-import
  :vc (org-pandoc-import :url "https://github.com/tecosaur/org-pandoc-import.git"))

(use-package org-anki
  :pin melpa
  :after org
  :custom
  (org-anki-default-deck "Default")
  (org-anki-default-match "@anki&todo<>\"TODO\"")
  (org-anki-inherit-tags nil))

(provide 'init-org)
