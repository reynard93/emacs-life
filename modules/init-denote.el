(use-package denote
  :demand t
  :init
  (setq denote-directory "~/src/notes/")

  ;; Setup denote-templates
  (defun denote-template-content (filename)
    "Read the contents of FILE and return as a string."
    (let ((denote-templates-directory (expand-file-name "templates" denote-directory)))
      (with-temp-buffer
        (insert-file-contents (expand-file-name filename denote-templates-directory))
        (buffer-string))))

  (setq denote-templates
        `((jira-ticket . ,(denote-template-content "jira-ticket.org"))))

  :config
  (message "denote is loaded")
  (denote-rename-buffer-mode 1)

  (require 'denote-org-extras)
  (require 'denote-journal-extras)
  (require 'denote-silo-extras)

  (require 'denote-org-extras)
  (setq denote-silo-extras-directories
        (list denote-directory
              "~/work/notes/"
              "~/work/openapply/notes/"))
  :custom
  (denote-history-completion-in-prompts nil)
  (denote-known-keywords '("emacs" "programming" "engineering" "parenting"))
  (denote-rename-buffer-format "[D] %t")
  (denote-journal-extras-title-format nil)

  :bind (("C-c n n" . denote)
         ("C-c n N" . denote-type)
         ("C-c n d" . denote-date)
         ("C-c n z" . denote-signature) ; "zettelkasten" mnemonic
         ("C-c n s" . denote-subdirectory)
         ("C-c n o" . denote-sort-dired) ; "order" mnemonic
         ("C-c n j" . denote-journal-extras-new-entry)
         ("C-c n J" . denote-journal-extras-new-or-existing-entry)
         ("C-c n S" . denote-silo-extras-select-silo-then-command)

         :map search-map
         ("n" . denote-open-or-create)
         ("N" . denote-silo-extras-open-or-create)

         :map text-mode-map
         ("C-c n i" . denote-link)
         ("C-c n I" . denote-add-links)
         ("C-c n b" . denote-backlinks)
         ("C-c n f f" . denote-find-link)
         ("C-c n f b" . denote-find-backlink)

         :map dired-mode-map
         ("C-c C-d C-i" . denote-link-dired-marked-notes)
         ("C-c C-d C-r" . denote-dired-rename-marked-files)
         ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
         ("C-c C-d C-f" . denote-dired-rename-marked-files-using-front-matter)))

(use-package consult-denote
  :demand t
  :after (consult denote)
  :config
  (message "consult-denote is loaded")
  (consult-denote-mode 1)
  :custom
  (consult-denote-grep-command #'consult-ripgrep)
  :bind ( :map search-map
          ("s" . consult-denote-grep)))

(use-package citar
  :pin melpa
  :init
  (setq org-cite-global-bibliography (list (expand-file-name "reference.bib" denote-directory)))
  (setq org-cite-insert-processor 'citar)
  (setq org-cite-follow-processor 'citar)
  (setq org-cite-activate-processor 'citar)
  :config
  (message "citar is loaded")
  :custom
  (citar-bibliography org-cite-global-bibliography)
  :bind (("C-c n c" . citar-open)
         ("C-c n C" . citar-create-note)))

(use-package citar-embark
  :pin melpa
  :after (citar embark)
  :config
  (message "citar-embark is loaded")
  (citar-embark-mode 1)
  :custom
  (citar-at-point-function #'embark-act))

(use-package citar-denote
  :pin melpa
  :demand t
  :after (:any citar denote)
  :config
  (message "citar-denote is loaded")
  (citar-denote-mode 1)
  :bind (("C-c w d" . citar-denote-dwim)
         ("C-c w n" . citar-denote-open-note)
         ("C-c w e" . citar-denote-open-reference-entry)
         ("C-c w a" . citar-denote-add-citekey)
         ("C-c w k" . citar-denote-remove-citekey)
         ("C-c w r" . citar-denote-find-reference)
         ("C-c w l" . citar-denote-link-reference)
         ("C-c w f" . citar-denote-find-citation)
         ("C-c w x" . citar-denote-nocite)
         ("C-c w y" . citar-denote-cite-nocite)
         ("C-c w z" . citar-denote-nobib)))

(use-package org-download
  :pin melpa
  :defer t
  :init
  (setq-default org-download-image-dir (expand-file-name "attachments/" denote-directory))
  :config
  (message "org-download is loaded"))

(provide 'init-denote)
