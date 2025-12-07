;;; -*- lexical-binding: t -*-

(use-package prot-modeline
  :ensure nil
  :load-path "vendor/"
  :custom
  (prot-modeline-string-truncate-length 50)
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
                  prot-modeline-eglot
                  "  "
                  mode-line-format-right-align
                  "  "
                  prot-modeline-misc-info)))

(provide 'init-modeline)
