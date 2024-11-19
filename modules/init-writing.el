(setq my-notes-directory (expand-file-name "notes/" my-src-directory))
(setq my-reference-file (expand-file-name "reference.bib" my-notes-directory))

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
  (("C-c w c c" . citar-create-note)
   ("C-c w c n" . citar-denote-open-note)
   :map text-mode-map
   ("C-c w c d" . citar-denote-dwim)
   ("C-c w c e" . citar-denote-open-reference-entry)
   ("C-c w c k" . citar-denote-add-citekey)
   ("C-c w c K" . citar-denote-remove-citekey))
  :custom
  (citar-denote-subdir "reference"))

(use-package denote
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n N" . denote-type)
   ("C-c n o" . denote-sort-dired)
   :map text-mode-map
   ("C-c n i" . denote-link-or-create)
   ("C-c n I" . denote-add-links)
   ("C-c n b" . denote-backlinks)
   ("C-c n r" . denote-rename-file)
   ("C-c n R" . denote-rename-file-using-front-matter)
   ("C-c n l l" . denote-find-link)
   ("C-c n l b" . denote-find-backlink)
   :map org-mode-map
   ("C-c n d l" . denote-org-extras-dblock-insert-links)
   ("C-c n d b" . denote-org-extras-dblock-insert-backlinks)
   :map dired-mode-map
   ("C-c C-d C-i" . denote-link-dired-marked-notes)
   ("C-c C-d C-r" . denote-dired-rename-marked-files)
   ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
   ("C-c C-d C-f" . denote-dired-rename-marked-files-using-front-matter))
  :custom
  (denote-directory my-notes-directory)
  (denote-known-keywords nil)
  :config
  (require 'denote-org-extras)
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
  :bind
  (;; Statistics
   ("C-c w x c" . denote-explore-count-notes)
   ("C-c w x C" . denote-explore-count-keywords)
   ("C-c w x b" . denote-explore-barchart-keywords)
   ("C-c w x e" . denote-explore-barchart-filetypes)
   ;; Random walks
   ("C-c w x r" . denote-explore-random-note)
   ("C-c w x l" . denote-explore-random-link)
   ("C-c w x k" . denote-explore-random-keyword)
   ("C-c w x x" . denote-explore-random-regex)
   ;; Denote Janitor
   ("C-c w x d" . denote-explore-identify-duplicate-notes)
   ("C-c w x z" . denote-explore-zero-keywords)
   ("C-c w x s" . denote-explore-single-keywords)
   ("C-c w x o" . denote-explore-sort-keywords)
   ("C-c w x w" . denote-explore-rename-keyword)
   ;; Visualise denote
   ("C-c w x n" . denote-explore-network)
   ("C-c w x v" . denote-explore-network-regenerate)
   ("C-c w x D" . denote-explore-degree-barchart)))

(provide 'init-writing)
