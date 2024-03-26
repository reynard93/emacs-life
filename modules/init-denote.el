(use-package denote
  :defer t
  :init
  (setq denote-directory "~/src/notes")

  (defun +denote/search ()
    (interactive)
    (+project/search denote-directory))

  (defun +denote/search-for-symbol-at-point ()
    (interactive)
    (+project/search denote-directory 'symbol))

  (defun +denote/scratch ()
    (interactive)
    (let ((denote-prompts '(keywords))
          (denote-file-type 'org))
      (call-interactively #'denote)))

  :config
  (message "denote is loaded")
  (require 'denote-org-extras)

  :custom
  (denote-history-completion-in-prompts nil)
  (denote-date-prompt-use-org-read-date t)
  (denote-known-keywords '("emacs" "nix" "ruby" "elixir" "webdev")))

(provide 'init-denote)
