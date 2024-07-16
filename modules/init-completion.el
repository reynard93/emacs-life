(use-package corfu
  :preface
  (setq tab-always-indent 'complete     ; Enable indentation+completion using the TAB key
        completion-cycle-threshold 3)   ; TAB cycle if there are only few candidates

  :config
  (message "corfu is loaded")
  (global-corfu-mode 1)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-quit-no-match t)
  (corfu-scroll-margin 5))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  :config
  (message "cape is loaded"))

(use-package tempel
  :init
  (defun +tempel/find-private-template ()
    (interactive)
    (let ((file (expand-file-name "templates/private.eld" user-emacs-directory)))
      (find-file file)))

  :config
  (message "tempel is loaded")
  (defun +tempel--setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  :custom
  (tempel-path (expand-file-name "templates/*.eld" user-emacs-directory))
  :hook
  ((prog-mode text-mode) . +tempel--setup-capf)
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert)))

(provide 'init-completion)
