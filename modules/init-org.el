(use-package org
  :ensure nil
  :init
  (setq org-directory "~/src/org"
        org-agenda-files (list org-directory))

  (defun +org/browse-files ()
    (interactive)
    (+project/browse-files org-directory))

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
    "Toggles last clocked item.

Clock out if an active clock is running (or cancel it if prefix ARG is non-nil).

If no clock is active, then clock into the last item. See `org-clock-in-last' to
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

  :config
  (message "org is loaded")

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
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-use-sub-superscripts nil)
  (org-export-with-sub-superscripts nil)
  (org-export-with-section-numbers nil)

  ;; Task
  (org-log-done 'time)
  (org-log-into-drawer t)
  (org-todo-keywords
   '((sequence "TODO(t)" "HOLD(h@/!)" "|" "DONE(d!)" "KILL(k@)")))
  (org-todo-keyword-faces
   '(("KILL" . (:inherit (italic org-warning)))
     ("HOLD" . (:inherit (italic org-warning)))))

  ;; Capture
  (org-capture-templates
   '(("t" "Tasks" entry (file "todo.org") "* TODO %?\n%i" :prepend t)
     ("n" "Notes" entry (file "notes.org") "* %?\n%i" :prepend t)
     ("j" "Journal" entry (file+olp+datetree "journal.org") "* %U %?\n%i")
     ("b" "Bookmark" entry (file+headline "notes.org" "Bookmarks") "* %?\n%x" :prepend t)
     ("a" "Anki")
     ("aa" "Default" entry (file "anki/default.org") "* %?\n%x" :prepend t)
     ("av" "Vocabulary" entry (file "anki/vocabulary.org") "* %?\n%x" :prepend t)))

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

  ;; Cite
  (org-cite-global-bibliography citar-bibliography)
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)

  :hook (org-capture-mode . evil-insert-state)

  :bind ( :map org-mode-map
          ("C-M-S-h" . org-babel-mark-block)
          ("C-c i" . org-cite-insert)))

(use-package ox-hugo
  :pin melpa
  :after ox
  :config
  (message "ox-hugo is loaded")
  :custom
  (org-hugo-delete-trailing-ws nil))

(use-package citar
  :pin melpa
  :defer t
  :init
  (setq citar-bibliography '("~/src/notes/reference.bib"))
  (setq citar-notes-paths '("~/src/notes/reference"))
  :config
  (message "citar is loaded"))

(use-package org-superstar
  :pin melpa
  :after org
  :config
  (message "org-superstar is loaded")
  :custom
  (org-hide-leading-stars nil)
  (org-superstar-leading-bullet ?\s)
  (org-indent-mode-turns-on-hiding-stars nil)
  :hook org-mode)

(use-package org-pandoc-import
  :after org
  :load-path "vendor/org-pandoc-import"
  :config
  (message "org-pandoc-import is loaded"))

(provide 'init-org)
