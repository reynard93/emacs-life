(defun yejun/browse-emacs-config ()
  (interactive)
  (+project/browse-files user-emacs-directory))

(defun yejun/browse-nix-config ()
  (interactive)
  (+project/browse-files "~/.config/nix-config"))

(defun yejun/browse-blog ()
  (interactive)
  (+project/browse-files "~/src/yejun.dev"))

(provide 'init-misc)
