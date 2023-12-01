(use-package org
  :defer t
  :commands (org-clock-goto org-clock-cancel)
  :preface
  (setq org-directory "~/src/org"
        org-agenda-files (list org-directory))

  (defun yejun/browse-org ()
    (interactive)
    (yejun/find-file-in-project org-directory))

  :config
  (message "org is loaded")
  (setq org-todo-keywords
        '((sequence "TODO(t)" "HOLD(h@/!)" "|" "DONE(d!)" "KILL(k@)")))
  (setq org-todo-keyword-faces
        '(("KILL" . (:inherit (italic org-warning)))
          ("HOLD" . (:inherit (italic org-warning)))))
  (setq org-capture-templates
        '(("t" "Tasks" entry (file "todo.org") "* TODO %?\n%i" :prepend t)
          ("j" "Journal" entry (file+olp+datetree "journal.org") "* %U %?\n%i")))

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

;; https://github.com/doomemacs/doomemacs/blob/986398504d09e585c7d1a8d73a6394024fe6f164/modules/lang/org/autoload/org.el#L318-L336
(defun yejun/toggle-last-clock (arg)
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

(use-package evil-org
  :pin melpa
  :after org
  :config
  (message "evil-org is loaded")
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  :hook (org-mode . (lambda () evil-org-mode)))

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
