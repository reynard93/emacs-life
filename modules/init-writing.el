(setq my-notes-directory (expand-file-name "notes/" my-src-directory))
(setq my-reference-file (expand-file-name "reference.bib" my-notes-directory))

(use-package ox-hugo
  :pin melpa
  :after org)

(use-package ox-gfm
  :pin melpa
  :after org)

(use-package citar
  :pin melpa
  :init
  (setq org-cite-global-bibliography `(,my-reference-file))
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
  (denote-known-keywords nil)
  (denote-org-capture-specifiers "%i\n%?")
  :config
  (require 'denote-org-extras)
  (require 'denote-journal-extras)
  (denote-rename-buffer-mode 1))

(use-package consult-denote
  :init
  (with-eval-after-load 'denote
    (consult-denote-mode 1))
  :bind
  (("C-c n f" . consult-denote-find)
   ("C-c n g" . consult-denote-grep))
  :custom
  (consult-denote-grep-command #'consult-ripgrep))

(use-package denote-explore
  :pin melpa
  :bind ("C-c n s" . denote-explore-sync-metadata))

(provide 'init-writing)
