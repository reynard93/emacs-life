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
     ("c" "Note" plain
      (file denote-last-path)
      #'denote-org-capture
      :no-save nil
      :immediate-finish nil
      :kill-buffer t
      :jump-to-captured nil)
     ("o" "OA" plain
      (file denote-last-path)
      (function
       (lambda ()
         (let ((denote-use-title (alfred-browser-title))
               (denote-use-keywords '("jira" "openapply")))
           (denote-org-capture))))
      :no-save nil
      :immediate-finish nil
      :kill-buffer t
      :jump-to-captured t)
     ("l" "TIL" plain
      (file denote-last-path)
      (function
       (lambda ()
         (let ((denote-use-template (concat
                                     "#+hugo_base_dir: ~/src/yejun.dev\n"
                                     "#+hugo_section: til")))
           (denote-org-capture-with-prompts :title :keywords))))
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
  :bind ("C-c n h" . my/org-hugo-denote-files-find-file)
  :custom
  (org-hugo-default-static-subdirectory-for-externals "attachments")
  (org-hugo-front-matter-format "yaml")
  :config
  (defun my/org-hugo-denote-files ()
    "Return a list of Hugo-compatible files in `denote-directory'."
    (let ((default-directory (denote-directory)))
      (process-lines "rg" "-l" "^#\\+hugo_base_dir" "--glob" "*.org")))

  (defun my/org-hugo-denote-files-find-file ()
    "Search Hugo-compatible files in `denote-directory' and visit the result."
    (interactive)
    (let* ((default-directory (denote-directory))
           (prompt (format "Select FILE in %s: "  default-directory))
           (selected-file (consult--read
                           (my/org-hugo-denote-files)
                           :state (consult--file-preview)
                           :history 'denote-file-history
                           :require-match t
                           :prompt prompt)))
      (find-file selected-file)))

  (defun my/org-hugo-export-all-denote-files ()
    "Export all Hugo-compatible files in `denote-directory'."
    (interactive)
    (let ((org-export-use-babel nil))
      (dolist (file (my/org-hugo-denote-files))
        (with-current-buffer (find-file-noselect file)
          (org-hugo-export-to-md))))))

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
  (("C-c w n" . citar-create-note)
   ("C-c w o" . citar-denote-open-note)
   ("C-c w d" . citar-denote-dwim)
   ("C-c w e" . citar-denote-open-reference-entry)
   ("C-c w a" . citar-denote-add-citekey)
   ("C-c w k" . citar-denote-remove-citekey)
   ("C-c w r" . citar-denote-find-reference)
   ("C-c w l" . citar-denote-link-reference)
   ("C-c w f" . citar-denote-find-citation)
   ("C-c w x" . citar-denote-nocite)
   ("C-c w y" . citar-denote-cite-nocite)
   ("C-c w z" . citar-denote-nobib))
  :custom
  (citar-denote-subdir "reference"))

(use-package denote
  :init
  (setq denote-directory my-notes-directory)
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote-open-or-create-with-command)
   ("C-c n o" . denote-sort-dired)
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
   ("C-c n F" . my/consult-denote-find-today)
   ("C-c n g" . consult-denote-grep))
  :custom
  (consult-denote-find-command #'consult-fd)
  (consult-denote-grep-command #'consult-ripgrep)
  :config
  (defun my/consult-denote-find-today ()
    "Call `consult-denote-find-command' in `denote-directory' using the current date."
    (declare (interactive-only t))
    (interactive)
    (let ((initial (format-time-string "%Y%m%d")))
      (funcall-interactively consult-denote-find-command
                             (denote-directory)
                             initial))))

(use-package denote-explore
  :pin melpa
  :bind ("C-c n s" . denote-explore-sync-metadata))

(provide 'init-writing)
