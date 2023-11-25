(setq package-enable-at-startup nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs started in %s" (emacs-init-time))))

(provide 'early-init)
