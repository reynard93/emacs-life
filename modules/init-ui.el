(use-package modus-themes
  :init
  (setq current-theme (if (display-graphic-p) 'modus-operandi 'modus-vivendi))
  :config
  (message "modus-themes is loaded")
  (load-theme current-theme :no-confirm)
  :custom
  (modus-themes-org-blocks 'gray-background)
  (modus-themes-completions
   '((matches . (extrabold underline))
     (selection . (semibold italic))))
  (modus-themes-headings
   '((1 . (variable-pitch 1.2))
     (2 . (variable-pitch 1.1))
     (3 . (variable-pitch 1.05))
     (4 . (1.0))
     (agenda-date . (1.2))
     (agenda-structure . (variable-pitch light 1.6))
     (t . (1.1)))))

(use-package spacious-padding
  :config
  (message "spacious-padding is loaded")
  (spacious-padding-mode 1)
  :custom
  (spacious-padding-subtle-mode-line t))

(use-package fontaine
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
  :hook (modus-themes-after-load-theme . fontaine-apply-current-preset)
  :bind ("C-c f" . fontaine-set-preset))

(provide 'init-ui)
