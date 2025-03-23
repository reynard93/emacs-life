(defvar my-notes-directory (expand-file-name "Notes/" my-src-directory))
;; (defvar my-notes-reference-file (expand-file-name "reference.bib" my-notes-directory)) ;; i am using the one configured by ews
(defvar my-notes-attachments-directory (expand-file-name "attachments/" my-notes-directory))
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("p" . "src python"))
(add-to-list 'org-structure-template-alist '("j" . "src java"))
(add-to-list 'org-structure-template-alist '("k" . "src kotlin"))
(add-to-list 'org-structure-template-alist '("sh" . "src sh"))
(add-to-list 'org-structure-template-alist '("rb" . "src ruby"))

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
  (org-directory "~/Notes")
  (org-default-notes-file "~/Notes/notes.org")
  (org-goto-interface 'outline-path-completion)
  (org-use-speed-commands t)
  (org-use-sub-superscripts '{})
  (org-M-RET-may-split-line '((default . nil)))
  (org-insert-heading-respect-content t)
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  (org-startup-with-inline-images t)
  (org-image-actual-width '(450))
  (org-fold-catch-invisible-edits 'show)
  ;; (org-pretty-entities t) ; THIS IS HORRIBLE FINALLY FOUND THE CULPRIT!

  ;; Agenda
  (org-agenda-files (list org-directory))
  (org-use-fast-todo-selection 'expert)

  ;; Capture
  (org-capture-templates
   `(("f" "Fleeting note" entry
      (file "notes.org")
      ,(concat "* %?\n"
               ":PROPERTIES:\n"
               ":CAPTURED: %U\n"
               ":END:"))
      ("p" "Permanent note" plain
       (file denote-last-path)
       #'denote-org-capture
       :no-save t
       :immediate-finish nil
       :kill-buffer t
       :jump-to-captured t)
      ("t" "New task" entry
      (file+headline org-default-notes-file "Tasks")
      "* TODO %i%?")
     ("r" "Reference note" plain
      (file denote-last-path)
      (function
       (lambda ()
         (let ((denote-use-title (alfred-browser-title)))
           (denote-org-capture-with-prompts :title :keywords))))
      :no-save nil
      :immediate-finish nil
      :kill-buffer t
      :jump-to-captured t)
     ("c" "Interstitial journaling" entry
      (file denote-journal-capture-entry-today)
      ,(concat "* %^{Thought} %^g\n"
               ":PROPERTIES:\n"
               ":CAPTURED: %U\n"
               ":END:")
      :no-save t
      :immediate-finish t
      :kill-buffer t
      :jump-to-captured nil
      )))

  ;; Code block
  (org-edit-src-content-indentation 0)
  (org-edit-src-persistent-message nil)
  (org-src-preserve-indentation t)
  (org-src-window-setup 'current-window)

  ;; Export
  (org-export-with-sub-superscripts '{})
  (org-export-dispatch-use-expert-ui t)
  (org-export-with-drawers nil)
  (org-export-with-todo-keywords nil)
  (org-export-with-toc nil)
  (org-export-with-smart-quotes t)
  (org-export-date-timestamp-format "%e %B %Y")

  ;; Log
  (org-log-into-drawer t)
  (org-log-done 'time)
  (org-log-redeadline 'note)
  (org-log-reschedule 'note)

  ;; Tag
  (org-auto-align-tags nil)
  (org-tags-column 0)

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (ruby . t)
     (dot . t)
     (python . t))))
;; Use GraphViz for flow diagrams
;; requires GraphViz software

(use-package ox-hugo
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
           (selected-file (consult--read
                           (my/org-hugo-denote-files)
                           :prompt (format "Select FILE in %s: "  default-directory)
                           :sort nil
                           :require-match t
                           :category 'file
                           :state (consult--file-preview)
                           :history 'denote-file-history)))
      (find-file selected-file)))

  (defun my/org-hugo-export-all-denote-files ()
    "Export all Hugo-compatible files in `denote-directory'."
    (interactive)
    (let ((org-export-use-babel nil))
      (dolist (file (my/org-hugo-denote-files))
        (with-current-buffer (find-file-noselect file)
          (org-hugo-export-to-md))))))

(use-package ox-gfm
  :ensure
  :after org)

(defcustom ews-bibtex-directory
  (concat (file-name-as-directory (getenv "HOME")) "library")
  "Location of BibTeX files and attachments."
  :group 'ews
  :type 'directory)
;;; BIBLIOGRAPHY
(defvar ews-bibtex-files
  (when (file-exists-p ews-bibtex-directory)
    (directory-files ews-bibtex-directory t "^[A-Z|a-z|0-9].+.bib$"))
  "List of BibTeX files. Use `ews-bibtex-register' to configure.")

(use-package citar
  :ensure
  :init
  (setq org-cite-global-bibliography `(,@ews-bibtex-files)) ; takes in a list
  (setq org-cite-insert-processor 'citar)
  (setq org-cite-follow-processor 'citar)
  (setq org-cite-activate-processor 'citar)
  :bind
  ("C-c w b o" . citar-open)
  :custom
  (citar-bibliography org-cite-global-bibliography)
  (citar-at-point-function #'embark-act))

(use-package citar-embark
  
  :after (citar embark)
  :config
  (citar-embark-mode 1))

(use-package citar-denote
  :after denote
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
   ("C-c w z" . citar-denote-nobib)))

(use-package denote
  :ensure
  :init
  (setq denote-directory my-notes-directory)
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n o" . denote-sort-dired)
   ("C-c n r" . denote-rename-file)
   :map text-mode-map
   ("C-c n i" . denote-link-or-create)
   ("C-c n I" . denote-add-links)
   ("C-c n b" . denote-backlinks)
   ("C-c n R" . denote-rename-file-using-front-matter)
   ("M-g l" . denote-find-link)
   ("M-g L" . denote-find-backlink)
   :map org-mode-map
   ("C-c n a" . my/denote-org-extras-insert-attachment)
   ("C-c n d l" . denote-org-extras-dblock-insert-links)
   ("C-c n d b" . denote-org-extras-dblock-insert-backlinks)
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
  (denote-rename-buffer-mode 1)
  (advice-add 'denote-link-ol-export :around
              (lambda (orig-fun link description format)
                (if (and (eq format 'md)
                         (eq org-export-current-backend 'hugo))
                    (let* ((path (denote-get-path-by-id link))
                           (export-file-name
                            (or
                             ;; Use export_file_name if it exists
                             (when (file-exists-p path)
                               (with-temp-buffer
                                 (insert-file-contents path)
                                 (goto-char (point-min))
                                 (when (re-search-forward "^#\\+export_file_name: \\(.+\\)" nil t)
                                   (match-string 1))))
                             ;; Otherwise, use the original file's base name
                             (file-name-nondirectory path))))
                      (format "[%s]({{< relref \"%s\" >}})"
                              description
                              export-file-name))
                  (funcall orig-fun link description format))))

  (defun my/denote-org-extras-insert-attachment (file)
    "Process FILE to use as an attachment in the current buffer.

If FILE is already in the attachments directory, simply insert a link to it.
Otherwise, rename it using `denote-rename-file' with a title derived from
the filename, move it to the attachments directory, and insert a link.

The link format used is '[[file:attachments/filename]]', following Org syntax.
This function is ideal for managing referenced files in note-taking workflows."
    (interactive (list (read-file-name "File: " my-notes-attachments-directory)))
    (let* ((orig-buffer (current-buffer))
           (attachments-dir my-notes-attachments-directory))

      ;; Check if the file is already in the attachments directory
      (if (string-prefix-p
           (file-name-as-directory attachments-dir)
           (expand-file-name file))

          ;; If already in attachments, just insert the link
          (with-current-buffer orig-buffer
            (insert (format "[[file:attachments/%s]]" (file-name-nondirectory file))))

        ;; Otherwise, rename and move the file
        (let ((title (denote-sluggify-title (file-name-base file))))
          (when-let* ((renamed-file (denote-rename-file file title))
                      (renamed-name (file-name-nondirectory renamed-file))
                      (final-path (expand-file-name renamed-name attachments-dir)))
            (rename-file renamed-file final-path t)
            (with-current-buffer orig-buffer
              (insert (format "[[file:attachments/%s]]" renamed-name))))))))

  ;; denote 3.2.0
  (defun denote-journal-extras-path-to-new-or-existing-entry (&optional date)
    "Return path to existing or new journal file.
With optional DATE, do it for that date, else do it for today.  DATE is
a string and has the same format as that covered in the documentation of
the `denote' function.  It is internally processed by
`denote-valid-date-p'.

If there are multiple journal entries for the date, prompt for one among
them using minibuffer completion.  If there is only one, return it.  If
there is no journal entry, create it."
    (let* ((internal-date (or (denote-valid-date-p date) (current-time)))
           (files (denote-journal-extras--entry-today internal-date)))
      (cond
       ((length> files 1)
        (completing-read "Select journal entry: " files nil t))
       (files
        (car files))
       (t
        (save-window-excursion
          (denote-journal-extras-new-entry date)
          (save-buffer)
          (buffer-file-name))))))

  (defun denote-journal-capture-entry-today ()
    "Capture to Denote Journal entry for today."
    (let ((date (format-time-string "%Y-%m-%d %H:%M:%S")))
      (setq denote-journal-capture-date date)
      (denote-journal-extras-path-to-new-or-existing-entry date))))

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
  
  :hook (org-capture-after-finalize . my/denote-explore-sync-metadata)
  :bind
  (;; bite to h prefix standing for hitchhike?
   ("C-c n s" . denote-explore-sync-metadata)
   ;; Statistics
   ("C-c h x c" . denote-explore-count-notes)
   ("C-c h x C" . denote-explore-count-keywords)
   ("C-c h x b" . denote-explore-barchart-keywords)
   ("C-c h x e" . denote-explore-barchart-filetypes)
   ;; Random walks
   ("C-c h x r" . denote-explore-random-note)
   ("C-c h x l" . denote-explore-random-link)
   ("C-c h x k" . denote-explore-random-keyword)
   ("C-c h x x" . denote-explore-random-regex)
   ;; Denote Janitor
   ("C-c h x d" . denote-explore-identify-duplicate-notes)
   ("C-c h x z" . denote-explore-zero-keywords)
   ("C-c h x s" . denote-explore-single-keywords)
   ("C-c h x o" . denote-explore-sort-keywords)
   ("C-c h x w" . denote-explore-rename-keyword)
   ;; Visualise denote
   ("C-c h x n" . denote-explore-network)
   ("C-c h x v" . denote-explore-network-regenerate)
   ("C-c h x D" . denote-explore-barchart-degree))
  :config
  (defun my/denote-explore-sync-metadata ()
    "Sync denote filenames when org-capture's target is a denote file.
It means the target is (file denote-last-path)."
    (let ((target-file (org-capture-get :target)))
      (when (and (listp target-file)
                 (eq (car target-file) 'file)
                 (eq (cadr target-file) 'denote-last-path))
        (denote-explore-sync-metadata)))))

;; Disable most features for beginners
(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  :custom
  (org-modern-table nil)
  (org-modern-keyword nil)
  (org-modern-timestamp nil)
  (org-modern-priority nil)
  (org-modern-checkbox nil)
  (org-modern-tag nil)
  (org-modern-block-name nil)
  (org-modern-keyword nil)
  (org-modern-footnote nil)
  (org-modern-internal-target nil)
  (org-modern-radio-target nil)
  (org-modern-statistics nil)
  (org-modern-progress nil)
  :config
  (setq org-modern-star '("◉" "○" "s◈" "◇" "*")))

(use-package denote-journal
  :ensure (:host github :repo "protesilaos/denote-journal")
  :bind
  (("C-c n j" . denote-journal-new-or-existing-entry)
   ("C-c n J" . denote-journal-new-entry)))

;; show hidden emphasis markers
(use-package org-appear
  :hook
  (org-mode . org-appear-mode))

;; LaTeX previews

(use-package org-fragtog
  :after org
  :hook
  (org-mode . org-fragtog-mode)
  :custom
  (org-startup-with-latex-preview nil)
  (org-format-latex-options
   (plist-put org-format-latex-options :scale 2)
   (plist-put org-format-latex-options :foreground 'auto)
   (plist-put org-format-latex-options :background 'auto)))


(provide 'init-writing)
