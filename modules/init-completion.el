(use-package corfu
  :init
  (global-corfu-mode 1)
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-quit-no-match t)
  (corfu-scroll-margin 5))

(use-package emacs
  :custom
  ;; Enable indentation+completion using the TAB key.
  (tab-always-indent 'complete)
  ;; TAB cycle if there are only few candidates
  (completion-cycle-threshold 3))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package tempel
  :init
  (setq tempel-path (expand-file-name "templates/*.eld" user-emacs-directory))
  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  :hook ((prog-mode text-mode) . tempel-setup-capf)
  :bind (("M-+" . tempel-complete)
         ("M-*" . tempel-insert)))

(provide 'init-completion)
