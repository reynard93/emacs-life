(use-package modus-themes
  :demand t
  :config
  (message "modus-themes is loaded")
  (modus-themes-load-theme
   (if (display-graphic-p)
       (if (+macos/dark-mode-p)
           'modus-vivendi
         'modus-operandi)
     'modus-vivendi))
  :custom
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t))

(use-package ef-themes
  :defer t
  :config
  (message "ef-themes is loaded")
  :custom
  (ef-themes-mixed-fonts t)
  (ef-themes-variable-pitch-ui t))

(use-package theme-buffet
  :if (display-graphic-p)
  :after (:any modus-themes ef-themes)
  :demand t
  :config
  (message "theme-buffet is loaded")
  (theme-buffet-timer-hours 1)
  (defun +theme-buffet/toggle ()
    (interactive)
    (if theme-buffet-timer-hours
        (theme-buffet--free-timer 'theme-buffet-timer-hours)
      (theme-buffet-timer-hours 1)))
  :custom
  (theme-buffet-menu 'end-user)
  (theme-buffet-end-user
   '( :night     (modus-vivendi ef-dark ef-winter ef-autumn ef-night ef-duo-dark ef-symbiosis)
      :morning   (modus-operandi ef-light ef-cyprus ef-spring ef-frost ef-duo-light)
      :afternoon (modus-operandi-tinted ef-arbutus ef-day ef-kassio ef-summer ef-elea-light ef-maris-light ef-melissa-light ef-trio-light ef-reverie)
      :evening   (modus-vivendi-tinted ef-rosa ef-elea-dark ef-maris-dark ef-melissa-dark ef-trio-dark ef-dream))))

(use-package fontaine
  :if (display-graphic-p)
  :demand t
  :config
  (message "fontaine is loaded")
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
  :hook (modus-themes-after-load-theme . fontaine-apply-current-preset)
  :bind ("C-c F" . fontaine-set-preset))

(provide 'init-ui)
