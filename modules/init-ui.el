(use-package modus-themes
  :init
  (load-theme 'modus-operandi :no-confirm)
  :bind ("<f9>" . modus-themes-toggle)
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t))

(use-package ef-themes
  :bind ("M-<f9>" . ef-themes-toggle)
  :custom
  (ef-themes-mixed-fonts t)
  (ef-themes-variable-pitch-ui t)
  (ef-themes-to-toggle '(ef-frost ef-bio)))

(use-package fontaine
  :if (display-graphic-p)
  :demand t
  :bind ("C-c F" . fontaine-set-preset)
  :custom
  (fontaine-presets
   '((small
      :default-height 120)
     (regular
      :default-height 160)
     (large
      :default-family "Aporetic Serif Mono"
      :default-weight semilight
      :default-height 180
      :fixed-pitch-family "Aporetic Serif Mono"
      :variable-pitch-family "Aporetic Sans"
      :bold-weight extrabold)
     (presentation
      :inherit large
      :default-height 260)
     (t
      :default-family "Aporetic Sans Mono"
      :default-weight regular
      :default-slant normal
      :default-width normal
      :default-height 100
      :fixed-pitch-family "Aporetic Sans Mono"
      :variable-pitch-family "Aporetic Serif")
     ))
  :config
  (fontaine-set-preset 'regular))

(use-package spacious-padding
  :if (display-graphic-p)
  :custom
  (spacious-padding-subtle-mode-line t)
  :config
  (spacious-padding-mode 1))

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
                  prot-modeline-flymake
                  "  "
                  prot-modeline-misc-info)))

(provide 'init-ui)
