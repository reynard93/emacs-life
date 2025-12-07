;;; -*- lexical-binding: t -*-

(use-package devdocs
  :defer t
  :hook
  (ruby-ts-mode . (lambda () (setq-local devdocs-current-docs '("ruby~3.3" "rails~8.0"))))
  ((web-mode haml-mode heex-ts-mode) . (lambda () (setq-local devdocs-current-docs '("html" "css" "javascript"))))
  :bind
  (:map search-map
        ("k" . devdocs-lookup)
        ("K" . devdocs-search))
  :config
  (defun my/devdocs-install ()
    "Download and install selected DevDocs documentations."
    (interactive)
    (let* ((available-docs (mapcar (lambda (doc) (alist-get 'slug doc))
                                   (devdocs--available-docs)))
           (selected-docs (completing-read-multiple "Install documentations: " available-docs)))
      (dolist (doc selected-docs)
        (devdocs-install doc)))))

(provide 'init-search)
