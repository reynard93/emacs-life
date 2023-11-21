(use-package popper
  :config
  (message "popper is loaded")
  (popper-mode 1)
  (popper-echo-mode 1)
  :custom
  (popper-reference-buffers
   '(compilation-mode
     rspec-compilation-mode
     chatgpt-shell-mode
     osx-dictionary-mode
     "*Org Select*"
     "^CAPTURE-.*\\.org$")))

(provide 'init-popups)
