(use-package transient
  :pin melpa
  :config
  (message "transient is loaded")

  ;; Magit
  (with-eval-after-load 'magit
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

(provide 'init-transient)
