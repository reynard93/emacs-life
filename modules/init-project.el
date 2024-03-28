(use-package project
  :ensure nil
  :defer t
  :config
  (message "project is loaded")
  :custom
  (project-switch-commands
   '((project-find-file "Find file")
     (deadgrep "Find regexp" "r")
     (project-find-dir "Find directory")
     (project-dired "Root dired")
     (+project/search "Search project" "s")
     (+project/search-for-symbol-at-point "Search project with symbol" "S")
     (magit-project-status "Git" "g")
     (project-eshell "Shell"))))

(defun yejun/browse-emacs-config ()
  (interactive)
  (+project/browse-files user-emacs-directory))

(defun yejun/browse-nix-config ()
  (interactive)
  (+project/browse-files "~/.config/nix-config"))

(defun yejun/browse-blog ()
  (interactive)
  (+project/browse-files "~/src/yejun.dev"))

(provide 'init-project)
