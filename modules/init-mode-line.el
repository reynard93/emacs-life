(use-package prot-modeline
  :ensure nil
  :load-path "lisp/prot-lisp"
  :config
  (setq-default mode-line-format
                '("%e"
                  prot-modeline-kbd-macro
                  prot-modeline-narrow
                  prot-modeline-buffer-status
                  prot-modeline-input-method
                  prot-modeline-evil
                  prot-modeline-buffer-identification
                  "  "
                  prot-modeline-major-mode
                  prot-modeline-process
                  "  "
                  prot-modeline-vc-branch
                  "  "
                  prot-modeline-eglot
                  "  "
                  prot-modeline-flymake
                  "  "
                  prot-modeline-misc-info)))

(use-package anzu
  :pin melpa
  :config
  (message "anzu is loaded")
  (global-anzu-mode 1))

(provide 'init-mode-line)
