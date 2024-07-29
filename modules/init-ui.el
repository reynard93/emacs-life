(use-package modus-themes
  :demand t
  :config
  (if (display-graphic-p)
      (load-theme 'modus-operandi :no-confirm)
    (load-theme 'modus-vivendi :no-confirm))
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t)
  :bind ("<f5>" . modus-themes-toggle))

(use-package fontaine
  :demand t
  :config
  (fontaine-set-preset 'regular)
  :custom
  (fontaine-presets
   '((regular)
     (presentation
      :default-height 240)
     (t
      :default-family "Iosevka Comfy"
      :default-weight regular
      :default-height 180
      :fixed-pitch-family "Iosevka Comfy"
      :fixed-pitch-weight nil
      :fixed-pitch-height 1.0
      :variable-pitch-family "Iosevka Comfy Motion Duo"
      :variable-pitch-weight nil
      :variable-pitch-height 1.0)
     ))
  :bind ("C-c F" . fontaine-set-preset))

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
                  prot-modeline-misc-info))
  :custom
  (prot-modeline-string-truncate-length 50))

(provide 'init-ui)
