(use-package denote
  :defer t
  :preface
  (setq denote-directory "~/src/notes")

  (defun yejun/search-notes ()
    (interactive)
    (yejun/search-project denote-directory))

  (defun yejun/search-notes-for-symbol-at-point ()
    (interactive)
    (yejun/search-project denote-directory 'symbol))

  :config
  (message "denote is loaded")

  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
                 '("n" "Note" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))

  :custom
  (denote-date-prompt-use-org-read-date t)
  (denote-known-keywords '("emacs" "nix" "ruby" "elixir" "webdev")))

(provide 'init-denote)
