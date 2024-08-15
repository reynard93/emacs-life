(use-package devdocs
  :hook
  (ruby-ts-mode . (lambda () (setq-local devdocs-current-docs '("ruby~3.3" "rails~7.0"))))
  (elixir-ts-mode . (lambda () (setq-local devdocs-current-docs '("elixir~1.17" "phoenix"))))
  ((web-mode haml-mode heex-ts-mode) . (lambda () (setq-local devdocs-current-docs '("html" "css" "javascript"))))
  :bind (:map search-map ("k" . devdocs-lookup))
  :config
  (defun +devdocs/install ()
    "Download and install multiple DevDocs documentations."
    (interactive)
    (let* ((available-docs (mapcar (lambda (doc) (alist-get 'slug doc))
                                   (devdocs--available-docs)))
           (selected-docs (completing-read-multiple "Install documentations: " available-docs)))
      (dolist (doc selected-docs)
        (devdocs-install doc)))))

(provide 'init-devdocs)
