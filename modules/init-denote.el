(use-package denote
  :defer 1
  :init
  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
                 '("n" "Note" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))
  :bind
  (("C-c n n" . denote)
   ("C-c n N" . denote-type)
   ("C-c n o" . denote-sort-dired)
   :map text-mode-map
   ("C-c n i" . denote-link-or-create)
   ("C-c n I" . denote-add-links)
   ("C-c n r" . denote-rename-file)
   ("C-c n R" . denote-rename-file-using-front-matter)
   ("C-c n f f" . denote-find-link)
   ("C-c n f b" . denote-find-backlink)
   :map org-mode-map
   ("C-c n d l" . denote-org-extras-dblock-insert-links)
   ("C-c n d b" . denote-org-extras-dblock-insert-backlinks)
   :map dired-mode-map
   ("C-c C-d C-i" . denote-link-dired-marked-notes)
   ("C-c C-d C-r" . denote-dired-rename-marked-files)
   ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
   ("C-c C-d C-f" . denote-dired-rename-marked-files-using-front-matter)
   :map search-map
   ("n" . denote-open-or-create)
   ("j" . denote-journal-extras-new-or-existing-entry))
  :custom
  (denote-directory (expand-file-name "notes/" my-src-directory))
  (denote-journal-extras-title-format 'day-date-month-year)
  (denote-known-keywords nil)
  :config
  (require 'denote-journal-extras)
  (require 'denote-org-extras)
  (denote-rename-buffer-mode 1))

(use-package consult-denote
  :init
  (with-eval-after-load 'denote
    (consult-denote-mode 1))
  :bind ("C-c n g" . consult-denote-grep)
  :custom
  (consult-denote-grep-command #'consult-ripgrep))

(provide 'init-denote)
