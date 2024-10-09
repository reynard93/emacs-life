(use-package corfu
  :init
  ;; Enable indentation+completion using the TAB key.
  (setq tab-always-indent 'complete)
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  :config
  (global-corfu-mode 1))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  :bind ("C-c p" . cape-prefix-map))

(use-package tempel
  :init
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  :hook ((prog-mode text-mode conf-mode) . tempel-setup-capf)
  :bind
  (("M-+" . tempel-complete)
   ("M-*" . tempel-insert))
  :custom
  (tempel-path (list (expand-file-name "templates/*.eld" user-emacs-directory)
                     "~/Library/CloudStorage/Dropbox/Emacs/private.eld"))
  :config
  (defun +tempel/find-template-file ()
    "List template files and open the selected one."
    (interactive)
    (when-let* ((template-files
                 (delete-dups
                  (mapcan (lambda (path)
                            (if (file-directory-p path)
                                (directory-files path t "\\.eld\\'")
                              (file-expand-wildcards path t)))
                          tempel-path)))
                (selected-file (completing-read "Open file: " template-files nil t)))
      (find-file selected-file))))

(provide 'init-completion)
