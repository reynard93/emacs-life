(use-package general
  :pin melpa
  :after evil
  :preface
  (defun yejun/browse-emacs-config ()
    (interactive)
    (+project/browse-files user-emacs-directory))

  (defun yejun/browse-nix-config ()
    (interactive)
    (+project/browse-files "~/.config/nix-config"))

  (defun yejun/browse-blog ()
    (interactive)
    (+project/browse-files "~/src/yejun.dev"))

  :config
  (message "general is loaded")

  (general-create-definer +evil/leader-key
    :states '(normal visual insert)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-c SPC")

  (general-create-definer +evil/local-leader-key
    :states '(normal visual insert)
    :prefix "SPC m"
    :non-normal-prefix "C-c SPC m")

  (+evil/leader-key
    "'"   #'vertico-repeat
    ","   #'project-find-file
    "."   #'find-file
    "/"   #'+project/search
    "*"   #'+project/search-for-symbol-at-point
    "`"   #'evil-switch-to-windows-last-buffer
    ";"   #'pp-eval-expression

    "SPC" #'project-switch-to-buffer
    "RET" #'bookmark-jump
    "a"   #'embark-act
    "x"   #'+denote/scratch
    "X"   #'org-capture

    ;; buffer/bookmark
    "bb"  #'switch-to-buffer
    "bB"  #'switch-to-buffer-other-window
    "bm"  #'bookmark-set
    "bM"  #'bookmark-delete
    "bz"  #'bury-buffer

    ;; code
    "cc"  #'compile
    "cC"  #'recompile
    "cf"  #'apheleia-format-buffer
    "cg"  #'+github/create-gist-region-or-buffer
    "cp"  #'+sourcehut/create-paste-region-or-buffer
    "cw"  #'delete-trailing-whitespace

    ;; file
    "fb"  #'yejun/browse-blog
    "fD"  #'+file/delete-this-file
    "fe"  #'yejun/browse-emacs-config
    "fn"  #'yejun/browse-nix-config
    "fp"  #'+tempel/find-private-template
    "fr"  #'recentf-open-files
    "fR"  #'+file/move-this-file
    "fy"  #'+buffer/yank-path
    "fY"  #'+buffer/yank-path-relative-to-project

    ;; git
    "gb"  #'magit-checkout
    "gB"  #'magit-blame-addition
    "gf"  #'magit-fetch
    "gF"  #'magit-pull
    "gg"  #'magit-status
    "gl"  #'magit-log-current
    "gL"  #'magit-log-buffer-file
    "gr"  #'git-gutter:revert-hunk
    "gs"  #'git-gutter:stage-hunk
    "gt"  #'git-timemachine-toggle

    ;; git - create
    "gcb" #'magit-branch-and-checkout
    "gcB" #'+git/create-backup-commit
    "gcp" #'+github/create-pull-request

    ;; git - open in browser
    "goo" #'browse-at-remote
    "gop" #'+github/browse-pull-request

    ;; notes
    "na"  #'consult-org-agenda
    "nA"  #'consult-org-heading
    "nb"  #'citar-open
    "nB"  #'org-babel-tangle
    "nc"  #'+org/toggle-last-clock
    "nC"  #'org-clock-goto
    "nf"  #'denote-open-or-create-with-command
    "nF"  #'+org/browse-files
    "nk"  #'denote-keywords-add
    "nK"  #'denote-keywords-remove
    "nm"  #'org-tags-view
    "nn"  #'denote
    "ns"  #'consult-denote-grep
    "nS"  #'org-search-view
    "nr"  #'denote-rename-file
    "nR"  #'denote-rename-file-using-front-matter
    "nt"  #'tmr
    "nT"  #'org-todo-list
    "nx"  #'+denote/template-with-subdirectory

    ;; open
    "oA"  #'org-agenda
    "oe"  #'elfeed
    "om"  #'mu4e
    "oM"  #'+mail/compose-for-service
    "oo"  #'+macos/reveal-in-finder
    "oO"  #'+macos/reveal-project-in-finder

    ;; password-store
    "pa"  #'password-store-otp-append
    "pA"  #'password-store-otp-append-from-image
    "pe"  #'password-store-edit
    "pi"  #'password-store-insert
    "pI"  #'password-store-otp-insert
    "pp"  #'password-store-copy
    "pP"  #'password-store-otp-token-copy
    "pr"  #'password-store-rename
    "pR"  #'password-store-remove
    "pu"  #'+pass/create-otp-key-uri

    ;; search
    "sd"  #'deadgrep
    "sg"  #'+lookup/google-translate-guess-source-lang
    "sG"  #'+lookup/google-translate-guess-source-lang-force-select
    "si"  #'consult-imenu
    "sI"  #'consult-imenu-multi
    "sk"  #'dash-at-point
    "sK"  #'dash-at-point-with-docset
    "sl"  #'ffap-menu
    "so"  #'+lookup/search-online
    "sr"  #'+buffer/search-multi
    "sR"  #'+buffer/search-multi-for-symbol-at-point
    "ss"  #'+buffer/search
    "sS"  #'+buffer/search-for-symbol-at-point
    "st"  #'osx-dictionary-search-word-at-point

    ;; toggle
    "tf"  #'flymake-mode
    "ts"  #'flyspell-mode
    "tw"  #'visual-line-mode
    "tz"  #'+zen/toggle

    ;; window
    "w"   evil-window-map
    "wm"  #'switch-to-minibuffer
    "wu"  #'winner-undo
    "wr"  #'winner-redo)

  (+evil/local-leader-key
    :keymaps 'ruby-ts-mode-map
    :major-modes t
    ;; bundle
    "bc" #'bundle-check
    "bC" #'bundle-console
    "bi" #'bundle-install
    "bu" #'bundle-update
    "be" #'bundle-exec
    "bo" #'bundle-open

    ;; rake
    "kk" #'rake
    "kr" #'rake-rerun
    "kR" #'rake-regenerate-cache
    "kf" #'rake-find-task)

  (+evil/local-leader-key
    :keymaps '(ruby-ts-mode-map rspec-mode-map rspec-compilation-mode-map)
    :major-modes t
    ;; test
    "ta" #'rspec-verify-all
    "ts" #'rspec-verify-single
    "tv" #'rspec-verify
    "tr" #'rspec-rerun
    "tl" #'rspec-run-last-failed
    "te" #'rspec-toggle-example-pendingness
    "tt" #'rspec-toggle-spec-and-target)

  (+evil/local-leader-key
    :keymaps 'org-mode-map
    :major-modes t
    "A"  #'org-archive-subtree
    "e"  #'org-export-dispatch
    "f"  #'org-footnote-action
    "h"  #'org-toggle-heading
    "i"  #'org-toggle-item
    "I"  #'org-id-get-create
    "k"  #'org-babel-remove-result
    "o"  #'org-set-property
    "p"  #'org-priority
    "q"  #'org-set-tags-command
    "t"  #'org-todo
    "T"  #'+org/preview-toggle
    "x"  #'org-toggle-checkbox

    ;; anki
    "aa" #'org-anki-sync-entry
    "aA" #'org-anki-sync-all
    "ab" #'org-anki-browse-entry
    "ac" #'org-anki-cloze-dwim

    ;; table
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

    ;; table - delete
    "bdc" #'org-table-delete-column
    "bdr" #'org-table-kill-row

    ;; table - insert
    "bic" #'org-table-insert-column
    "bih" #'org-table-insert-hline
    "biH" #'org-table-hline-and-move
    "bir" #'org-table-insert-row

    ;; clock
    "ci" #'org-clock-in
    "co" #'org-clock-out
    "cR" #'org-clock-report

    ;; date/deadline
    "dd" #'org-deadline
    "ds" #'org-schedule
    "dt" #'org-time-stamp
    "dT" #'org-time-stamp-inactive

    ;; goto
    "gr" #'org-refile-goto-last-stored
    "gx" #'org-capture-goto-last-stored

    ;; links
    "la" #'denote-add-links
    "lh" #'denote-org-extras-link-to-heading
    "ll" #'denote-link-to-existing-or-new-note
    "lt" #'org-toggle-link-display

    ;; refile
    "r." #'+org/refile-to-current-file
    "rf" #'+org/refile-to-file
    "rr" #'org-refile
    "rR" #'org-refile-reverse

    ;; subtree
    "sd" #'org-cut-subtree
    "ss" #'org-sparse-tree
    "sS" #'org-sort)

  (+evil/local-leader-key
    :keymaps 'nix-mode-map
    :major-modes t
    "t" #'+nix/formatter-toggle)

  (+evil/local-leader-key
    :keymaps '(markdown-mode-map markdown-view-mode-map)
    :major-modes t
    "T" #'+markdown/preview-toggle))

(provide 'init-evil-keybindings)
