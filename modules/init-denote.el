(use-package denote
  :init
  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
                 '("f" "Fleeting note" plain
                   (file denote-last-path)
                   (function
                    (lambda ()
                      (let ((denote-directory (concat denote-directory "fleeting/")))
                        (denote-org-capture-with-prompts :title :keywords))))
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))
  :bind
  (("C-c n n" . denote)
   ("C-c n N" . denote-type)
   ("C-c n d" . denote-date)
   ("C-c n z" . denote-signature) ; "zettelkasten" mnemonic
   ("C-c n s" . denote-subdirectory)
   ("C-c n o" . denote-sort-dired) ; "order" mnemonic
   ("C-c n j" . denote-journal-extras-new-entry)
   ("C-c n J" . denote-journal-extras-new-or-existing-entry)
   ("C-c x"   . (lambda () (interactive) (org-capture nil "f")))
   :map search-map
   ("n" . denote-open-or-create)
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
   ("C-c C-d C-f" . denote-dired-rename-marked-files-using-front-matter))

  :custom
  (denote-directory "~/src/notes/")
  (denote-known-keywords '("emacs" "programming" "education"))

  :config
  (require 'denote-journal-extras)
  (require 'denote-org-extras)
  (denote-rename-buffer-mode 1))

(use-package consult-denote
  :bind (:map search-map ("m" . consult-denote-grep))
  :custom
  (consult-denote-grep-command #'consult-ripgrep)
  :config
  (consult-denote-mode 1))

(provide 'init-denote)
