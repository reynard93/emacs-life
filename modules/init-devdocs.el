(use-package devdocs
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
