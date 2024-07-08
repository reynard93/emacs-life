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

  (defun +denote/scratch ()
    (interactive)
    (let ((denote-prompts nil)
          (denote-file-type 'text))
      (call-interactively #'denote)))

  :custom
  (denote-history-completion-in-prompts nil)
  (denote-known-keywords '("emacs" "programming" "parenting"))
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
         ("s-n"     . +denote/scratch)

         :map search-map
         ("f" . denote-open-or-create)

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
  :after denote
  :config
  (message "consult-denote is loaded")
  (consult-denote-mode 1)
  :custom
  (consult-denote-grep-command #'consult-ripgrep)
  :bind ( :map search-map
          ("n" . consult-denote-grep)))

(use-package citar
  :pin melpa
  :init
  (setq org-cite-global-bibliography '("~/src/notes/reference.bib"))
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

(use-package org-download
  :pin melpa
  :defer t
  :init
  (setq-default org-download-image-dir (expand-file-name "attachments/" denote-directory))
  :config
  (message "org-download is loaded"))

(provide 'init-denote)
