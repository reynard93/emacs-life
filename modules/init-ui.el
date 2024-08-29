(use-package modus-themes
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t)
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t))

(use-package ef-themes
  :custom
  (ef-themes-mixed-fonts t)
  (ef-themes-variable-pitch-ui t)
  (ef-themes-to-toggle '(ef-summer ef-winter)))

(use-package theme-buffet
  :after (modus-themes ef-themes)
  :custom
  (theme-buffet-menu 'end-user)
  (theme-buffet-end-user
   '( :night (modus-vivendi ef-owl)
      :morning (modus-operandi ef-eagle)
      :afternoon (modus-operandi-tinted ef-summer)
      :evening (modus-vivendi-tinted ef-winter)))
  :config
  (if (display-graphic-p)
      (progn
        (theme-buffet-a-la-carte)
        (theme-buffet-timer-hours 2))
    (load-theme 'modus-vivendi :no-confirm)))

(use-package fontaine
  :if (display-graphic-p)
  :demand t
  :bind ("C-c F" . fontaine-set-preset)
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
