(use-package general
  :pin melpa
  :after evil
  :config
  (message "general is loaded")

  (general-create-definer +evil/leader-key
    :states '(normal visual)
    :keymaps 'override
    :prefix "SPC")

  (general-create-definer +evil/local-leader-key
    :states '(normal visual)
    :prefix "SPC m")

  (+evil/leader-key
    "p"   project-prefix-map
    "h"   help-map

    "'"   #'vertico-repeat
    ","   #'switch-to-buffer
    "."   #'find-file
    "/"   #'+project/search
    "*"   #'+project/search-for-symbol-at-point
    "`"   #'evil-switch-to-windows-last-buffer
    ";"   #'pp-eval-expression

    "SPC" #'project-switch-to-buffer
    "RET" #'bookmark-jump
    "a"   #'embark-act
    "x"   #'org-store-link
    "X"   #'org-capture

    "b"   '(:ignore t :which-key "buffer")
    "bb"  #'switch-to-buffer
    "bB"  #'switch-to-buffer-other-window
    "bd"  #'dired-jump
    "bi"  #'ibuffer
    "bm"  #'bookmark-set
    "bM"  #'bookmark-delete
    "bn"  #'evil-buffer-new
    "bN"  #'evil-window-new
    "br"  #'revert-buffer
    "bR"  #'rename-buffer
    "bs"  #'basic-save-buffer
    "bS"  #'evil-write-all
    "bz"  #'bury-buffer

    "c"   '(:ignore t :which-key "code")
    "cc"  #'compile
    "cC"  #'recompile
    "cf"  #'apheleia-format-buffer
    "cg"  #'+github/create-gist-region-or-buffer
    "cp"  #'+sourcehut/create-paste-region-or-buffer
    "cw"  #'delete-trailing-whitespace

    "f"   '(:ignore t :which-key "file")
    "fD"  #'+file/delete-this-file
    "fe"  #'yejun/browse-emacs-config
    "fn"  #'yejun/browse-nix-config
    "fo"  #'+macos/reveal-in-finder
    "fO"  #'+macos/reveal-project-in-finder
    "fr"  #'recentf-open-files
    "fR"  #'+file/move-this-file
    "fs"  #'save-buffer
    "fS"  #'write-file
    "fy"  #'+buffer/yank-path
    "fY"  #'+buffer/yank-path-relative-to-project

    "g"   '(:ignore t :which-key "git")
    "g/"  #'magit-dispatch
    "g."  #'magit-file-dispatch
    "g["  #'git-gutter:previous-hunk
    "g]"  #'git-gutter:next-hunk
    "gb"  #'magit-branch-checkout
    "gB"  #'magit-blame-addition
    "gC"  #'magit-clone
    "gd"  #'magit-dired-jump
    "gD"  #'magit-file-delete
    "gf"  #'magit-fetch
    "gF"  #'magit-pull
    "gg"  #'magit-status
    "gG"  #'magit-status-here
    "gl"  #'magit-log-current
    "gL"  #'magit-log-buffer-file
    "gr"  #'git-gutter:revert-hunk
    "gR"  #'vc-revert
    "gs"  #'git-gutter:stage-hunk
    "gS"  #'magit-stage-file
    "gt"  #'git-timemachine-toggle
    "gU"  #'magit-unstage-file

    "gc"  '(:ignore t :which-key "create")
    "gcb" #'magit-branch-and-checkout
    "gcB" #'+git/create-backup-commit
    "gcp" #'+github/create-pull-request
    "gcr" #'magit-init

    "go"  '(:ignore t :which-key "open in browser")
    "goo" #'browse-at-remote
    "gop" #'+github/browse-pull-request

    "n"   '(:ignore t :which-key "notes")
    "na"  #'consult-org-agenda
    "nb"  #'citar-open
    "nB"  #'citar-open-notes
    "nc"  #'+org/toggle-last-clock
    "nC"  #'org-clock-goto
    "nd"  #'denote-subdirectory
    "nD"  #'denote-date
    "nf"  #'denote-open-or-create-with-command
    "nF"  #'+org/browse-files
    "nl"  #'denote-link
    "nn"  #'+denote/scratch
    "ns"  #'+denote/search
    "nS"  #'+denote/search-for-symbol-at-point
    "nr"  #'denote-rename-file
    "nR"  #'denote-rename-file-using-front-matter
    "nt"  #'denote-template
    "nT"  #'org-todo-list

    "o"   '(:ignore t :which-key "open")
    "oA"  #'org-agenda
    "oe"  #'eshell
    "om"  #'mu4e
    "ot"  #'tmr

    "oa"  '(:ignore t :which-key "org agenda")
    "oat" #'org-todo-list
    "oam" #'org-tags-view
    "oas" #'org-search-view

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
    "opu" #'+pass/create-otp-key-uri

    "q"   '(:ignore t :which-key "quit")
    "qK"  #'save-buffers-kill-emacs
    "qq"  #'save-buffers-kill-terminal
    "qQ"  #'evil-quit-all-with-error-code

    "s"   '(:ignore t :which-key "search")
    "sd"  #'deadgrep
    "sg"  #'+lookup/google-translate-guess-source-lang
    "sG"  #'+lookup/google-translate-guess-source-lang-force-select
    "si"  #'consult-imenu
    "sI"  #'consult-imenu-multi
    "sk"  #'dash-at-point
    "sK"  #'dash-at-point-with-docset
    "sl"  #'ffap-menu
    "so"  #'+lookup/search-online
    "ss"  #'+buffer/search
    "sS"  #'+buffer/search-for-symbol-at-point
    "sr"  #'+project/search
    "sR"  #'+project/search-for-symbol-at-point
    "st"  #'osx-dictionary-search-word-at-point

    "t"   '(:ignore t :which-key "toggle")
    "tf"  '(flymake-mode :which-key "Flymake mode")
    "tl"  '(logos-focus-mode :which-key "Logos-Focus mode")
    "tm"  '(modus-themes-toggle :which-key "Modus themes")
    "to"  '(olivetti-mode :which-key "Olivetti mode")

    "w"   '(evil-window-map :which-key "window")
    "wu"  #'winner-undo
    "wr"  #'winner-redo)

  (+evil/local-leader-key
    :keymaps 'ruby-ts-mode-map
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

  (+evil/local-leader-key
    :keymaps '(ruby-ts-mode-map rspec-mode-map rspec-compilation-mode-map)
    :major-modes t
    "t"  '(:ignore t :which-key "test")
    "ta" #'rspec-verify-all
    "ts" #'rspec-verify-single
    "tv" #'rspec-verify
    "tr" #'rspec-rerun
    "tl" #'rspec-run-last-failed
    "te" #'rspec-toggle-example-pendingness)

  (+evil/local-leader-key
    :keymaps 'org-mode-map
    :major-modes t
    "." #'consult-org-heading
    "A" #'org-archive-subtree
    "e" #'org-export-dispatch
    "f" #'org-footnote-action
    "h" #'org-toggle-heading
    "i" #'org-toggle-item
    "I" #'org-id-get-create
    "k" #'org-babel-remove-result
    "o" #'org-set-property
    "p" #'org-priority
    "q" #'org-set-tags-command
    "t" #'org-todo
    "T" #'org-todo-list
    "x" #'org-toggle-checkbox

    "b"  '(:ignore t :which-key "table")
    "b-" #'org-table-insert-hline
    "ba" #'org-table-align
    "bb" #'org-table-blank-field
    "bc" #'org-table-create-or-convert-from-region
    "be" #'org-table-edit-field
    "bf" #'org-table-edit-formulas
    "bh" #'org-table-field-info
    "bs" #'org-table-sort-lines
    "br" #'org-table-recalculate
    "bR" #'org-table-recalculate-buffer-tables

    "bd"  '(:ignore t :which-key "delete")
    "bdc" #'org-table-delete-column
    "bdr" #'org-table-kill-row

    "bi"  '(:ignore t :which-key "insert")
    "bic" #'org-table-insert-column
    "bih" #'org-table-insert-hline
    "biH" #'org-table-hline-and-move
    "bir" #'org-table-insert-row

    "c"  '(:ignore t :which-key "clock")
    "ci" #'org-clock-in
    "co" #'org-clock-out
    "cR" #'org-clock-report

    "d"  '(:ignore t :which-key "date/deadline")
    "dd" #'org-deadline
    "ds" #'org-schedule
    "dt" #'org-time-stamp
    "dT" #'org-time-stamp-inactive

    "g"  '(:ignore t :which-key "goto")
    "gr" #'org-refile-goto-last-stored
    "gx" #'org-capture-goto-last-stored

    "l"  '(:ignore t :which-key "links")
    "lt" #'org-toggle-link-display

    "r"  '(:ignore t :which-key "refile")
    "r." #'+org/refile-to-current-file
    "rf" #'+org/refile-to-file
    "rr" #'org-refile
    "rR" #'org-refile-reverse

    "s"  '(:ignore t :which-key "subtree")
    "sd" #'org-cut-subtree
    "ss" #'org-sparse-tree
    "sS" #'org-sort)

  (+evil/local-leader-key
    :keymaps 'nix-mode-map
    :major-modes t
    "t" #'+nix/toggle-formatter))

(defun yejun/browse-emacs-config ()
  (interactive)
  (+project/browse-files "~/.config/emacs"))

(defun yejun/browse-nix-config ()
  (interactive)
  (+project/browse-files "~/.config/nix-config/"))

(provide 'init-evil-keybindings)
