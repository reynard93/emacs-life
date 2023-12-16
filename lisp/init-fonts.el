(use-package fontaine
  :when (display-graphic-p)
  :demand t
  :config
  (message "fontaine is loaded")
  (fontaine-set-preset 'regular)
  :custom
  (fontaine-presets
   '((regular)
     (presentation
      :default-weight semilight
      :default-height 240
      :bold-weight extrabold)
     (t
      :default-family "JetBrains Mono"
      :default-weight regular
      :default-height 160
      :fixed-pitch-family nil
      :fixed-pitch-weight nil
      :fixed-pitch-height 1.0
      :variable-pitch-family "Iosevka"
      :variable-pitch-weight regular
      :variable-pitch-height 1.0)))
  :bind ("C-c f" . fontaine-set-preset)
  :hook (modus-themes-after-load-theme . fontaine-apply-current-preset))

(provide 'init-fonts)
