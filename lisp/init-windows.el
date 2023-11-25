(use-package winner
  :ensure nil
  :config
  (message "winner is loaded")
  (winner-mode 1))

(use-package ace-window
  :pin melpa
  :config
  (message "ace-window is loaded")
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind ([remap other-window] . ace-window))

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

(provide 'init-windows)
