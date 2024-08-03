(use-package magit
  :pin melpa
  :bind
  (("C-c g b" . magit-checkout)
   ("C-c g B" . magit-blame-addition)
   ("C-c g f" . magit-fetch)
   ("C-c g F" . magit-pull)
   ("C-c g l" . magit-log-current)
   ("C-c g L" . magit-log-buffer-file))

  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  :config
  (with-eval-after-load 'transient
    ;; git push with skip-ci option
    (transient-append-suffix 'magit-push "-n"
      '("-s" "Skip CI" "--push-option=skip-ci"))

    ;; git push to all remotes
    (defun +magit/push-all (&optional args)
      (interactive (list (magit-push-arguments)))
      (dolist (remote (magit-list-remotes))
        (magit-push-to-remote remote args)))
    (transient-append-suffix 'magit-push "e"
      '("E" "everywhere" +magit/push-all))))

(use-package browse-at-remote
  :pin melpa
  :bind ("C-c g o" . browse-at-remote)
  :custom
  (browse-at-remote-add-line-number-if-no-region-selected nil))

(use-package gh
  :ensure nil
  :load-path "site-lisp/"
  :init
  (defvar-keymap embark-gh-pr-map
    "c" #'gh-pr-checkout
    "o" #'gh-pr-browse
    "v" #'gh-pr-view
    "y" #'gh-pr-link)
  (with-eval-after-load 'embark
    (add-to-list 'embark-keymap-alist '(github-pull-request . embark-gh-pr-map)))
  (with-eval-after-load 'marginalia
    (add-to-list 'marginalia-prompt-categories '("Select pull request" . github-pull-request)))
  :bind
  (("C-c g v" . gh-pr-view)
   ("C-c g c" . gh-pr-create)
   :map embark-region-map
   ("G" . gh-gist-create)))

(provide 'init-git)
