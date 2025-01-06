(use-package nix-mode
  :pin nongnu
  :hook (before-save . nix-format-before-save)
  :config
  (defun org-babel-execute:nix (body params)
    (setq strict-option (if (assoc :strict params) "--strict" ""))
    (with-temp-buffer
      (insert body)
      (shell-command-on-region
       (point-min) (point-max)
       (concat "nix-instantiate --eval " strict-option " - <<EOF\n$(cat)\nEOF")
       (current-buffer)
       t)
      (buffer-string))))

(use-package org-nix-shell
  :pin melpa
  :hook org-mode)

(provide 'init-nix)
