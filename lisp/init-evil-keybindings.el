(use-package general
  :pin melpa
  :config
  (message "general is loaded")

  (general-create-definer yejun/leader-key
    :states '(normal visual)
    :keymaps 'override
    :prefix "SPC")

  (general-create-definer yejun/local-leader-key
    :states '(normal visual)
    :prefix "SPC m")

  (yejun/leader-key
    "u"   #'universal-argument

    "p"   project-prefix-map
    "w"   evil-window-map
    "h"   help-map

    "'"   #'vertico-repeat
    ","   #'project-find-file
    "."   #'project-find-dir
    "/"   #'yejun/search-project
    "*"   #'yejun/search-project-for-symbol-at-point
    "`"   #'evil-switch-to-windows-last-buffer

    "SPC" #'project-switch-to-buffer
    "RET" #'bookmark-jump
    "x"   #'org-capture

    "b"   '(:ignore t :which-key "buffer")
    "bb"  #'switch-to-buffer
    "bB"  #'switch-to-buffer-other-window
    "bi"  #'ibuffer
    "bk"  #'kill-current-buffer
    "bm"  #'bookmark-set
    "bM"  #'bookmark-delete
    "bn"  #'evil-buffer-new
    "br"  #'revert-buffer
    "bR"  #'rename-buffer
    "bs"  #'basic-save-buffer
    "bw"  #'evil-write-all
    "bz"  #'bury-buffer

    "c"   '(:ignore t :which-key "code")
    "cc"  #'compile
    "cC"  #'recompile
    "cg"  #'yejun/gist-region-or-buffer
    "cp"  #'yejun/paste-region-or-buffer
    "cw"  #'delete-trailing-whitespace

    "f"   '(:ignore t :which-key "file")
    "fb"  #'yejun/browse-blog
    "fe"  #'yejun/browse-emacs-config
    "fn"  #'yejun/browse-nix-config
    "fs"  #'save-buffer
    "fS"  #'write-file
    "fr"  #'recentf-open-files

    "g"   '(:ignore t :which-key "git")
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

    "go"  '(:ignore t :which-key "open in browser")
    "goo" #'browse-at-remote
    "goc" #'forge-browse-commit
    "goi" #'forge-browse-issue
    "goI" #'forge-browse-issues
    "gop" #'forge-browse-pullreq
    "goP" #'forge-browse-pullreqs

    "gc"  '(:ignore t :which-key "create")
    "gci" #'forge-create-issue
    "gcp" #'forge-create-pullreq

    "n"   '(:ignore t :which-key "notes")
    "na"  #'org-agenda
    "nb"  #'citar-open
    "nB"  #'citar-open-notes
    "nd"  #'denote-date
    "nf"  #'yejun/browse-notes
    "nF"  #'yejun/browse-org
    "nl"  #'denote-link
    "nn"  #'denote
    "nN"  #'denote-type
    "nr"  #'denote-subdirectory
    "ns"  #'yejun/search-notes
    "nS"  #'yejun/search-notes-for-symbol-at-point
    "nt"  #'denote-template

    "o"   '(:ignore t :which-key "open")
    "oe"  #'eshell
    "of"  #'make-frame
    "oF"  #'select-frame-by-name
    "om"  #'mu4e
    "oo"  #'yejun/show-current-file-in-finder

    "op"  '(:ignore t :which-key "pass")
    "opa" #'password-store-otp-append
    "opA" #'password-store-otp-append-from-image
    "ope" #'password-store-edit
    "opi" #'password-store-insert
    "opI" #'password-store-otp-insert
    "opp" #'password-store-copy
    "opP" #'password-store-otp-token-copy
    "opr" #'password-store-rename
    "opR" #'password-store-remove
    "opu" #'yejun/otp-key-uri

    "q"   '(:ignore t :which-key "quit")
    "qf"  #'delete-frame
    "qK"  #'save-buffers-kill-emacs
    "qq"  #'save-buffers-kill-terminal
    "qQ"  #'evil-quit-all-with-error-code

    "s"   '(:ignore t :which-key "search")
    "si"  #'consult-imenu
    "sI"  #'consult-imenu-multi
    "sl"  #'ffap-menu
    "ss"  #'yejun/search-buffer
    "sS"  #'yejun/search-buffer-for-symbol-at-point
    "sr"  #'yejun/search-project
    "sR"  #'yejun/search-project-for-symbol-at-point

    "t"   '(:ignore t :which-key "toggle")
    "tf"  '(flymake-mode :which-key "Flymake")
    "tn"  '(yejun/toggle-nix-formatter :which-key "Nix formatter")
    "tr"  '(read-only-mode :which-key "Read-only mode")
    "tv"  '(visible-mode :which-key "Visible mode"))

  (yejun/local-leader-key
    :keymaps 'ruby-mode-map
    :major-modes t
    "b"  '(:ignore t :which-key "bundle")
    "bc" #'bundle-check
    "bC" #'bundle-console
    "bi" #'bundle-install
    "bu" #'bundle-update
    "be" #'bundle-exec
    "bo" #'bundle-open

    "k"  '(:ignore t :which-key "rake")
    "kk" #'rake
    "kr" #'rake-rerun
    "kR" #'rake-regenerate-cache
    "kf" #'rake-find-task)

  (yejun/local-leader-key
    :keymaps 'rspec-mode-map
    :major-modes t
    "t"  '(:ignore t :which-key "test")
    "ta" #'rspec-verify-all
    "ts" #'rspec-verify-single
    "tv" #'rspec-verify
    "tr" #'rspec-rerun
    "tl" #'rspec-run-last-failed
    "te" #'rspec-toggle-example-pendingness)

  (yejun/local-leader-key
    :keymaps 'org-mode-map
    :major-modes t
    "e"  #'org-export-dispatch)
  )

(defun yejun/browse-project (dir)
  (let ((project (project-current nil dir)))
    (project-find-file-in nil nil project)))

(defun yejun/browse-emacs-config ()
  (interactive)
  (yejun/browse-project "~/.config/emacs"))

(defun yejun/browse-nix-config ()
  (interactive)
  (yejun/browse-project "~/.config/nix-config/"))

(defun yejun/browse-blog ()
  (interactive)
  (yejun/browse-project "~/src/yejun.dev"))

(defun yejun/show-current-file-in-finder ()
  (interactive)
  (shell-command (concat "open -R " (shell-quote-argument (buffer-file-name)))))

(defun yejun/native-compile-packages ()
  (interactive)
  (let ((package-directory (expand-file-name "elpa" user-emacs-directory)))
    (native-compile-async package-directory 'recursively)))

(provide 'init-evil-keybindings)
