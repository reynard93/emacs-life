(defvar my-notes-directory (expand-file-name "notes/" my-src-directory))
(defvar my-notes-reference-file (expand-file-name "reference.bib" my-notes-directory))

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
  (org-use-sub-superqscripts '{})
  (org-M-RET-may-split-line '((default . nil)))
  (org-insert-heading-respect-content t)

  ;; Agenda
  (org-agenda-files (list org-directory))
  (org-use-fast-todo-selection 'expert)
  (org-todo-keywords
   '((sequence "TODO(t)" "WAIT(w!)" "|" "CANCEL(c!)" "DONE(d!)")))

  ;; Capture
  (org-capture-templates
   `(("t" "Task" entry
      (file "tasks.org")
      "* TODO %?")
     ("c" "Fleeting note" plain
      (file denote-last-path)
      (function
       (lambda ()
         (let ((denote-use-keywords '("fleeting")))
           (denote-org-capture-with-prompts nil))))
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
  (org-export-dispatch-use-expert-ui t)

  ;; Log
  (org-log-into-drawer t)
  (org-log-done 'time)
  (org-log-redeadline 'note)
  (org-log-reschedule 'note)

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (racket . t)
     (shell . t)
     (ruby . t)
     (python . t))))

(use-package org-download
  :pin melpa
  :after org
  :custom
  (org-download-image-dir "attachments")
  (org-download-display-inline-images nil)
  (org-download-heading-lvl nil)
  :config
  (setq org-download-file-format-function #'my/org-download-file-format-default)
  (defun my/org-download-file-format-default (filename)
    (let ((identifier (denote-create-unique-file-identifier filename))
          (title (denote-sluggify-title (file-name-base filename)))
          (extension (file-name-extension filename)))
      (concat identifier "--" title "." extension))))

(use-package org-anki
  :pin melpa
  :after org
  :custom
  (org-anki-default-deck "Default")
  (org-anki-default-match "@anki&todo<>\"TODO\"")
  (org-anki-inherit-tags nil))

(use-package ox-hugo
  :pin melpa
  :after org
  :custom
  (org-hugo-default-static-subdirectory-for-externals "attachments"))

(use-package ox-gfm
  :pin melpa
  :after org)

(use-package citar
  :pin melpa
  :init
  (setq org-cite-global-bibliography `(,my-notes-reference-file))
  (setq org-cite-insert-processor 'citar)
  (setq org-cite-follow-processor 'citar)
  (setq org-cite-activate-processor 'citar)
  :custom
  (citar-bibliography org-cite-global-bibliography)
  (citar-at-point-function #'embark-act))

(use-package citar-embark
  :pin melpa
  :after (citar embark)
  :config
  (citar-embark-mode 1))

(use-package citar-denote
  :pin melpa
  :init
  (with-eval-after-load 'citar
    (citar-denote-mode 1))
  :bind
  (("C-c n c" . citar-create-note)
   ("C-c n o" . citar-denote-open-note)
   ("C-c n ." . citar-denote-dwim))
  :custom
  (citar-denote-subdir "reference"))

(use-package denote
  :init
  (setq denote-directory my-notes-directory)
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote-open-or-create-with-command)
   ("C-c n d" . denote-sort-dired)
   ("C-c n r" . denote-rename-file)
   ("C-c n z" . denote-rename-file-signature)
   ("C-c n j" . denote-journal-extras-new-entry)
   :map org-mode-map
   ("C-c n i" . denote-link-or-create)
   ("C-c n I" . denote-org-extras-link-to-heading)
   ("C-c n b" . denote-backlinks)
   ("C-c n R" . denote-rename-file-using-front-matter)
   ("M-g l" . denote-find-link)
   ("M-g L" . denote-find-backlink)
   :map dired-mode-map
   ("C-c C-d C-i" . denote-link-dired-marked-notes)
   ("C-c C-d C-r" . denote-dired-rename-marked-files)
   ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
   ("C-c C-d C-f" . denote-dired-rename-marked-files-using-front-matter))
  :custom
  (denote-save-buffers t)
  (denote-known-keywords nil)
  (denote-org-capture-specifiers "%i\n%?")
  :config
  (require 'denote-org-extras)
  (require 'denote-journal-extras)
  (denote-rename-buffer-mode 1)
  (advice-add 'denote-link-ol-export :around
              (lambda (orig-fun link description format)
                (if (and (eq format 'md)
                         (eq org-export-current-backend 'hugo))
                    (let ((path (denote-get-path-by-id link)))
                      (format "[%s]({{< relref \"%s\" >}})"
                              description
                              (denote-sluggify-title
                               (denote-retrieve-filename-title path))))
                  (funcall orig-fun link description format)))))

(use-package consult-denote
  :init
  (with-eval-after-load 'denote
    (consult-denote-mode 1))
  :bind
  (("C-c n f" . consult-denote-find)
   ("C-c n g" . consult-denote-grep))
  :custom
  (consult-denote-find-command #'consult-fd)
  (consult-denote-grep-command #'consult-ripgrep))

(use-package denote-explore
  :pin melpa
  :bind ("C-c n s" . denote-explore-sync-metadata))

(provide 'init-writing)
