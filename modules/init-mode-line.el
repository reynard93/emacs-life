(use-package prot-modeline
  :ensure nil
  :load-path "vendor/prot-lisp"
  :init
  (defvar-local yejun-modeline-position
      '(:eval
        (when (mode-line-window-selected-p)
          (list
           (propertize "%l" 'face 'font-lock-number-face) ":"
           (propertize "%c" 'face 'font-lock-number-face)))))
  (put 'yejun-modeline-position 'risky-local-variable t)
  :config
  (setq-default mode-line-format
                '("%e"
                  prot-modeline-kbd-macro
                  prot-modeline-narrow
                  prot-modeline-buffer-status
                  prot-modeline-window-dedicated-status
                  prot-modeline-input-method
                  "  "
                  prot-modeline-buffer-identification
                  "  "
                  prot-modeline-major-mode
                  prot-modeline-process
                  "  "
                  prot-modeline-vc-branch
                  "  "
                  yejun-modeline-position
                  "  "
                  prot-modeline-eglot
                  "  "
                  prot-modeline-flymake
                  "  "
                  prot-modeline-notmuch-indicator
                  "  "
                  prot-modeline-misc-info))
  :custom
  (prot-modeline-string-truncate-length 50))

(use-package anzu
  :pin melpa
  :config
  (message "anzu is loaded")
  (global-anzu-mode 1))

(provide 'init-mode-line)
