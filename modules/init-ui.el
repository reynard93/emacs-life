(use-package modus-themes
  :config
  (message "modus-themes is loaded")
  (modus-themes-load-theme
   (if (display-graphic-p)
       (if (+macos/dark-mode-p)
           'modus-vivendi
         'modus-operandi)
     'modus-vivendi))
  :custom
  (modus-themes-custom-auto-reload nil)
  (modus-themes-mixed-fonts t)
  (modus-themes-variable-pitch-ui t)
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs nil)
  (modus-themes-completions
   '((matches . (underline))
     (selection . (extrabold))))
  (modus-themes-prompts '(extrabold))
  (modus-themes-headings
   '((agenda-structure . (variable-pitch light 1.8))
     (agenda-date . (1.3))
     (t . (regular 1.0)))))

(use-package ef-themes
  :defer t
  :config
  (message "ef-themes is loaded")
  :custom
  (ef-themes-to-toggle '(ef-summer ef-winter))
  (ef-themes-mixed-fonts t)
  (ef-themes-variable-pitch-ui t))

(use-package theme-buffet
  :if (display-graphic-p)
  :after (:any modus-themes ef-themes)
  :defer 1
  :config
  (message "theme-buffet is loaded")
  (theme-buffet-timer-hours 1)
  :custom
  (theme-buffet-menu 'end-user)
  (theme-buffet-end-user
   '( :night     (modus-vivendi ef-dark ef-winter ef-autumn ef-night ef-duo-dark ef-symbiosis)
      :morning   (modus-operandi ef-light ef-cyprus ef-spring ef-frost ef-duo-light)
      :afternoon (modus-operandi-tinted ef-arbutus ef-day ef-kassio ef-summer ef-elea-light ef-maris-light ef-melissa-light ef-trio-light ef-reverie)
      :evening   (modus-vivendi-tinted ef-rosa ef-elea-dark ef-maris-dark ef-melissa-dark ef-trio-dark ef-dream))))

(use-package spacious-padding
  :if (display-graphic-p)
  :config
  (message "spacious-padding is loaded")
  (spacious-padding-mode 1)
  :custom
  (spacious-padding-subtle-mode-line t))

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
  :bind ("C-c f" . fontaine-set-preset))

(use-package logos
  :if (display-graphic-p)
  :init
  (setq-default logos-hide-cursor nil
                logos-hide-mode-line t
                logos-hide-buffer-boundaries t
                logos-hide-fringe t
                logos-variable-pitch nil
                logos-buffer-read-only nil
                logos-scroll-lock nil
                logos-olivetti t)
  :config
  (message "logos is loaded")
  :custom
  (logos-outlines-are-pages t)
  :bind (([remap narrow-to-region] . logos-narrow-dwim)
         ([remap forward-page]     . logos-forward-page-dwim)
         ([remap backward-page]    . logos-backward-page-dwim)
         ("M-]" . logos-forward-page-dwim)
         ("M-[" . logos-backward-page-dwim)))

(use-package olivetti
  :pin melpa
  :defer t
  :config
  (message "olivetti is loaded")
  :custom
  (olivetti-body-width 0.7)
  (olivetti-minimum-body-width 80)
  (olivetti-recall-visual-line-mode-entry-state t))

(provide 'init-ui)
