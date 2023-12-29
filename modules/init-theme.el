(use-package modus-themes
  :init
  (setq current-theme (if (display-graphic-p) 'modus-operandi 'modus-vivendi))
  :config
  (message "modus-themes is loaded")
  (load-theme current-theme :no-confirm)
  :custom
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
  (spacious-padding-mode 1))

(use-package logos
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
         ([remap backward-page]    . logos-backward-page-dwim)))

(use-package olivetti
  :pin melpa
  :after logos
  :config
  (message "olivetti is loaded")
  :custom
  (olivetti-body-width 80))

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
  :bind ("C-c f" . fontaine-set-preset)
  :hook (modus-themes-after-load-theme . fontaine-apply-current-preset))

(use-package pulsar
  :config
  (message "pulsar is loaded")
  (pulsar-global-mode 1)
  :custom
  (pulsar-face 'pulsar-magenta)
  (pulsar-pulse-functions
   '(ace-window
     delete-window
     delete-other-windows
     evil-goto-line
     evil-goto-first-line
     evil-goto-last-change
     evil-scroll-page-down
     evil-scroll-page-up
     evil-scroll-line-to-bottom
     evil-scroll-line-to-center
     evil-scroll-line-to-top
     evil-window-prev
     evil-window-next
     evil-window-left
     evil-window-right
     evil-window-up
     evil-window-down
     evil-window-split
     evil-window-vsplit
     tab-new
     tab-close
     tab-next
     tab-previous
     widen)))

(provide 'init-theme)
