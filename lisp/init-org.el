(use-package org
  :commands (org-clock-goto org-clock-cancel)
  :preface
  (setq org-directory "~/src/org"
        org-agenda-files (list org-directory))

  (defun +org/browse-files ()
    (interactive)
    (+project/browse-files org-directory))

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

  ;; https://github.com/doomemacs/doomemacs/blob/4d072ce888577b023774460f6036abefcd0a1fa6/modules/lang/org/config.el#L132
  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (org-agenda-files :maxlevel . 3))
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)

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

  :config
  (message "org is loaded")
  (evil-define-key '(normal visual) 'global (kbd "RET") #'org-open-at-point)

  (setq org-todo-keywords
        '((sequence "TODO(t)" "HOLD(h@/!)" "|" "DONE(d!)" "KILL(k@)")))
  (setq org-todo-keyword-faces
        '(("KILL" . (:inherit (italic org-warning)))
          ("HOLD" . (:inherit (italic org-warning)))))
  (setq org-capture-templates
        '(("t" "Tasks" entry (file "todo.org") "* TODO %?\n%i" :prepend t)
          ("j" "Journal" entry (file+olp+datetree "journal.org") "* %U %?\n%i")))

  ;; Advices
  (defun move-to-eol-advice (&rest args) (end-of-line))
  (advice-add 'org-meta-return :before #'move-to-eol-advice)
  (advice-add 'org-insert-todo-heading :before #'move-to-eol-advice)

  :custom
  (org-startup-indented t)
  (org-src-preserve-indentation t)
  (org-confirm-babel-evaluate nil)
  (org-src-window-setup 'other-window)
  (org-log-into-drawer t)
  (org-log-done 'time)

  :bind ( :map org-mode-map
          ("C-M-S-h" . org-babel-mark-block))

  :hook (org-capture-mode . evil-insert-state))

(use-package evil-org
  :pin melpa
  :after (evil org)
  :config
  (message "evil-org is loaded")
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  :hook org-mode)

(use-package ob
  :ensure nil
  :after org
  :config
  (message "ob is loaded")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t))))

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

(use-package org-cite
  :ensure nil
  :after org
  :custom
  (org-cite-global-bibliography citar-bibliography)
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  :bind ( :map org-mode-map
          ("C-c i" . org-cite-insert)))

(provide 'init-org)
