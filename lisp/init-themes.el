(use-package modus-themes
  :config
  (message "modus-themes is loaded")
  (let ((theme (if (display-graphic-p) 'modus-operandi 'modus-vivendi)))
    (load-theme theme :no-confirm))
  :custom
  (modus-themes-completions
   '((matches . (extrabold underline))
     (selection . (semibold italic)))))

(provide 'init-themes)
