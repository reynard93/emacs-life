(use-package nix-mode
  :pin melpa
  :config
  (message "nix-mode is loaded")

  (defun yejun/toggle-nix-formatter ()
    (interactive)
    (if (string-match "nixfmt" nix-nixfmt-bin)
        (setq nix-nixfmt-bin "nixpkgs-fmt")
      (setq nix-nixfmt-bin "nixfmt"))
    (message (concat "Switched nix-nixfmt-bin to " nix-nixfmt-bin)))

  (defun nix-formatter-mode-line-display ()
    (add-to-list 'mode-line-process '(:eval nix-nixfmt-bin)))

  :custom
  (nix-nixfmt-bin "nixfmt")

  :hook
  (nix-mode . nix-formatter-mode-line-display)
  (before-save . nix-format-before-save))

(defun org-babel-execute:nix (body params)
  (setq strict-option (if (assoc :strict params) "--strict" ""))
  (with-temp-buffer
    (insert body)
    (shell-command-on-region
     (point-min) (point-max)
     (concat "nix-instantiate --eval " strict-option " - <<EOF\n$(cat)\nEOF")
     (current-buffer)
     t)
    (buffer-string)))

(use-package envrc
  :pin melpa
  :config
  (message "envrc is loaded")
  (envrc-global-mode 1))

(provide 'init-nix)
