(use-package tab-bar
  :ensure nil
  :when (display-graphic-p)
  :config
  (message "tab-bar is loaded")
  ;; bind s-1 through s-9 to switch tabs
  (dolist (i (number-sequence 1 9))
    (global-set-key (kbd (format "s-%d" i))
                    `(lambda ()
                       (interactive)
                       (when (<= ,i (length (tab-bar-tabs)))
                         (tab-bar-select-tab ,i)))))

  :custom
  (tab-bar-show 1)

  :bind (("s-t" . tab-new)
         ("s-T" . tab-undo)
         ("s-w" . tab-close)
         ("s-W" . tab-close-group)
         ("s-g" . tab-switch)
         ("s-G" . tab-group)
         ("s-{" . tab-previous)
         ("s-}" . tab-next)
         ("s-}" . tab-next)
         ("C-<tab>" . tab-recent)))

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

(use-package spacious-padding
  :when (display-graphic-p)
  :config
  (spacious-padding-mode 1))

(use-package beframe
  :when (display-graphic-p)
  :config
  (message "beframe is loaded")
  (beframe-mode 1)
  :bind-keymap ("C-c b" . beframe-prefix-map))

(use-package server
  :when (display-graphic-p)
  :ensure nil
  :defer 1
  :init
  (setq server-name "gui")
  :config
  (message "server is loaded")
  (unless (server-running-p)
    (server-start)))

(provide 'init-gui-frames)
