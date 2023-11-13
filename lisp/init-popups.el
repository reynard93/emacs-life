(use-package popwin
  :pin melpa
  :config
  (message "popwin is loaded")
  (popwin-mode 1)
  (push '("*scratch*" :stick t) popwin:special-display-config)
  (push "*Org Select*" popwin:special-display-config)
  (push '("^CAPTURE-.*\\.org$" :regexp t :stick t) popwin:special-display-config)
  (push '(chatgpt-shell-mode :height 0.5 :stick t) popwin:special-display-config)
  (push '(rspec-compilation-mode :height 0.3 :stick t) popwin:special-display-config)
  (push '(osx-dictionary-mode :height 0.3 :stick t) popwin:special-display-config))

(provide 'init-popups)
