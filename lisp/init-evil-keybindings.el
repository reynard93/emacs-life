(use-package general
  :pin melpa
  :config
  (message "general is loaded")

  (general-create-definer yejun/leader-key
    :states '(normal visual insert emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC")

  (yejun/leader-key
    "u"   #'universal-argument

    "p"   project-prefix-map
    "w"   evil-window-map
    "h"   help-map

    "."   #'find-file
    ","   #'switch-to-buffer
    "`"   #'evil-switch-to-windows-last-buffer
    "'"   #'vertico-repeat
    "x"   #'org-capture

    "SPC" #'project-find-file
    "RET" #'bookmark-jump

    "b"   '(:ignore b :which-key "buffer")
    "bb"  #'switch-to-buffer
    "bB"  #'switch-to-buffer-other-window
    "bm"  #'bookmark-set
    "bM"  #'bookmark-delete
    "bn"  #'evil-buffer-new
    "br"  #'revert-buffer
    "bR"  #'rename-buffer
    "bs"  #'basic-save-buffer
    "bw"  #'evil-write-all
    "bz"  #'bury-buffer

    "c"   '(:ignore c :which-key "code")
    "cc"  #'compile
    "cC"  #'recompile
    "cg"  #'yejun/gist-region-or-buffer
    "cp"  #'yejun/paste-region-or-buffer
    "cw"  #'delete-trailing-whitespace

    "f"   '(:ignore f :which-key "file")
    "fr"  #'consult-recent-file
    "fs"  #'save-buffer
    "fS"  #'write-file

    "g"   '(:ignore g :which-key "git")
    "g/"  #'magit-dispatch
    "g."  #'magit-file-dispatch
    "g'"  #'forge-dispatch
    "gb"  #'magit-branch-checkout
    "gB"  #'magit-blame-addition
    "gD"  #'magit-file-delete
    "gg"  #'magit-status
    "gG"  #'magit-status-here
    "gL"  #'magit-log-buffer-file
    "gR"  #'vc-revert
    "gS"  #'magit-stage-file
    "gU"  #'magit-unstage-file

    "go"  '(:ignore o :which-key "open in browser")
    "goo" #'browse-at-remote
    "goc" #'forge-browse-commit
    "goi" #'forge-browse-issue
    "goI" #'forge-browse-issues
    "gop" #'forge-browse-pullreq
    "goP" #'forge-browse-pullreqs

    "gc"  '(:ignore c :which-key "create")
    "gci" #'forge-create-issue
    "gcp" #'forge-create-pullreq

    "s"   '(:ignore t :which-key "search")
    "si"  #'consult-imenu
    "sI"  #'consult-imenu-multi
    "sl"  #'ffap-menu
    "sr"  #'consult-ripgrep
    "ss"  #'consult-line
    "sS"  #'yejun/consult-line-at-point

    "t"   '(:ignore t :which-key "toggle")
    "tf"  '(flymake-mode :which-key "Flymake")
    "tn"  '(yejun/toggle-nix-formatter :which-key "Nix formatter")
    "tr"  '(read-only-mode :which-key "Read-only mode")
    "tv"  '(visible-mode :which-key "Visible mode")
  ))

(provide 'init-evil-keybindings)
