(use-package fontaine
  :when (display-graphic-p)
  :demand t
  :config
  (message "fontaine is loaded")
  (fontaine-set-preset 'default)
  :custom
  (fontaine-presets
   '((default
      :default-height 160)
     (presentation
      :default-height 240
      :default-weight semilight
      :bold-weight extrabold)
     (t
      :default-family "JetBrains Mono"
      :default-weight regular
      :variable-pitch-family "Iosevka"
      :variable-pitch-weight regular
      :variable-pitch-height 1.0)))
  :bind ("C-c f" . fontaine-set-preset)
  :hook (modus-themes-after-load-theme . fontaine-apply-current-preset))

(provide 'init-fonts)
