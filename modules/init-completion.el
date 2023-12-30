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
  (message "cape is loaded")
  :bind ( :prefix-map cape-prefix-map
          :prefix "C-c p"
          ("p" . completion-at-point)
          ("t" . complete-tag)
          ("d" . cape-dabbrev)
          ("h" . cape-history)
          ("f" . cape-file)
          ("k" . cape-keyword)
          ("s" . cape-elisp-symbol)
          ("e" . cape-elisp-block)
          ("a" . cape-abbrev)
          ("l" . cape-line)
          ("w" . cape-dict)
          (":" . cape-emoji)
          ("\\" . cape-tex)
          ("_" . cape-tex)
          ("^" . cape-tex)
          ("&" . cape-sgml)
          ("r" . cape-rfc1345)))

(use-package tempel
  :config
  (message "tempel is loaded")

  (defun tempel-setup-capf ()
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  :custom
  (tempel-trigger-prefix "<")
  :hook
  ((prog-mode text-mode) . tempel-setup-capf))

(use-package tempel-collection
  :pin melpa
  :after tempel
  :config
  (message "tempel-collection is loaded"))

(provide 'init-completion)
