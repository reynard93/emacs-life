(use-package denote
  :init
  (setq denote-directory "~/src/notes")
  :config
  (message "denote is loaded")
  :custom
  (denote-date-prompt-use-org-read-date t)
  (denote-known-keywords '("emacs" "nix" "ruby" "elixir" "webdev"))
  :bind (("C-c n n" . denote)
         ("C-c n N" . denote-type)
         ("C-c n d" . denote-date)
         ("C-c n D" . denote-subdirectory)
         ("C-c n t" . denote-template)
         ("C-c n l" . denote-link)
         ("C-c n f" . yejun/browse-notes)
         ("C-c n s" . yejun/search-notes)))

(defun yejun/browse-notes ()
  (interactive)
  (let ((project (project-current nil denote-directory)))
    (project-find-file-in nil nil project)))

(defun yejun/search-notes ()
  (interactive)
  (consult-ripgrep denote-directory (thing-at-point 'symbol)))

(provide 'init-denote)
