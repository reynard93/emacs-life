(use-package org
  :ensure nil
  :bind
  (("C-c l" . org-store-link)
   ("C-c c" . org-capture)
   :map org-mode-map
   ([remap mark-defun] . org-babel-mark-block)
   ("M-g o" . consult-org-heading))

  :custom
  ;; Display
  (org-ellipsis "…")
  (org-use-sub-superscripts '{})

  ;; Movement
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)

  ;; Code block
  (org-src-preserve-indentation t)

  ;; Export
  (org-export-dispatch-use-expert-ui t)
  (org-export-with-sub-superscripts '{})

  ;; Logging
  (org-log-done 'time)
  (org-log-into-drawer t)

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
