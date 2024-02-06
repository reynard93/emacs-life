(use-package nix-mode
  :pin melpa
  :init
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

  :init
  (setq nix-nixfmt-bin "nixfmt")
  (defun +nix/toggle-formatter ()
    (interactive)
    (if (string-match "nixfmt" nix-nixfmt-bin)
        (setq nix-nixfmt-bin "nixpkgs-fmt")
      (setq nix-nixfmt-bin "nixfmt"))
    (message (concat "Switched nix-nixfmt-bin to " nix-nixfmt-bin)))

  :config
  (message "nix-mode is loaded")
  (defun +nix--formatter-mode-line-display ()
    (add-to-list 'mode-line-process '(:eval (concat " (" nix-nixfmt-bin ")"))))

  :hook
  (nix-mode . +nix--formatter-mode-line-display)
  (before-save . nix-format-before-save))

(provide 'init-nix)
