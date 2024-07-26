(use-package modus-themes
  :demand t
  :config
  (load-theme 'modus-vivendi :no-confirm)
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

(provide 'init-ui)
