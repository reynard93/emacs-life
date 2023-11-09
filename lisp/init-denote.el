(use-package denote
  :defer t
  :init
  (setq denote-directory "~/src/notes")
  :config
  (message "denote is loaded")
  :custom
  (denote-date-prompt-use-org-read-date t)
  (denote-known-keywords '("emacs" "nix" "ruby" "elixir" "webdev")))

(defun yejun/browse-notes ()
  (interactive)
  (yejun/find-file-in-project denote-directory))

(defun yejun/search-notes ()
  (interactive)
  (yejun/search-in-project denote-directory))

(defun yejun/search-notes-for-symbol-at-point ()
  (interactive)
  (yejun/search-in-project denote-directory 'symbol))

(provide 'init-denote)
