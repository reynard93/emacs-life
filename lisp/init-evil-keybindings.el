(use-package evil
  :pin melpa
  :demand t
  :init
  (setq evil-normal-state-cursor 'box
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow)
  :config
  (message "evil is loaded")
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-mode 1)
  :custom
  (evil-want-keybinding nil)            ; required by evil-collection
  (evil-kill-on-visual-paste nil)       ; avoid adding replaced text to kill-ring
  ;; undo
  (evil-undo-system 'undo-redo)         ; required by `evil-redo'
  (evil-want-fine-undo t)
  ;; copy
  (evil-visual-update-x-selection-p nil)
  ;; search
  (evil-symbol-word-search t)
  (evil-ex-visual-char-range t)
  ;; replace
  (evil-ex-substitute-global t)
  ;; move
  (evil-move-cursor-back nil)
  (evil-move-beyond-eol nil)
  ;; window
  (evil-vsplit-window-right t)
  (evil-split-window-below t)
  :bind ( :map evil-ex-completion-map
          ("C-a" . evil-beginning-of-line)
          ("C-b" . evil-backward-char)
          ("C-f" . evil-forward-char)
          :map evil-ex-search-keymap
          ("C-a" . evil-beginning-of-line)
          ("C-b" . evil-backward-char)
          ("C-f" . evil-forward-char)))

(use-package evil-collection
  :pin melpa
  :after evil
  :config
  (message "evil-collection is loaded")
  (evil-collection-init '(calendar dired ediff eglot flymake replace xref)))

(use-package general
  :pin melpa
  :after evil
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
    "a"   #'embark-act

    "p"   project-prefix-map
    "h"   help-map

    "'"   #'vertico-repeat
    "."   #'popper-cycle
    ","   #'popper-toggle
    "/"   #'yejun/search-project
    "*"   #'yejun/search-project-for-symbol-at-point
    "`"   #'evil-switch-to-windows-last-buffer
    ";"   #'pp-eval-expression

    "SPC" #'project-switch-to-buffer
    "RET" #'bookmark-jump
    "x"   #'+denote/scratch
    "X"   #'org-capture

    "b"   '(:ignore t :which-key "buffer")
    "bb"  #'switch-to-buffer
    "bB"  #'switch-to-buffer-other-window
    "bd"  #'dired-jump
    "bi"  #'ibuffer
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
    "fD"  #'yejun/delete-this-file
    "fe"  #'yejun/browse-emacs-config
    "fn"  #'yejun/browse-nix-config
    "fs"  #'save-buffer
    "fS"  #'write-file
    "fr"  #'recentf-open-files
    "fy"  #'yejun/yank-buffer-path
    "fY"  #'yejun/yank-buffer-path-relative-to-project

    "g"   '(:ignore t :which-key "git")
    "g,"  #'magit-file-dispatch
    "g."  #'magit-dispatch
    "g["  #'git-gutter:previous-hunk
    "g]"  #'git-gutter:next-hunk
    "gb"  #'magit-branch-checkout
    "gB"  #'magit-blame-addition
    "gd"  #'magit-dired-jump
    "gD"  #'magit-file-delete
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

    "go"  '(:ignore t :which-key "open in browser")
    "goo" #'browse-at-remote

    "n"   '(:ignore t :which-key "notes")
    "na"  #'org-agenda
    "nb"  #'citar-open
    "nB"  #'citar-open-notes
    "nc"  #'+org/toggle-last-clock
    "nC"  #'org-clock-cancel
    "nd"  #'denote-date
    "nD"  #'denote-subdirectory
    "nf"  #'denote-open-or-create-with-command
    "nF"  #'yejun/browse-org
    "nl"  #'denote-link
    "nn"  #'denote
    "nN"  #'denote-type
    "no"  #'org-clock-goto
    "ns"  #'+denote/search
    "nS"  #'+denote/search-for-symbol-at-point
    "nr"  #'denote-change-file-type-and-front-matter
    "nR"  #'denote-rename-file-using-front-matter
    "nt"  #'denote-template
    "nT"  #'org-todo-list

    "o"   '(:ignore t :which-key "open")

    "oa"  '(:ignore t :which-key "org agenda")
    "oat" #'org-todo-list
    "oam" #'org-tags-view
    "oas" #'org-search-view

    "oA"  #'org-agenda
    "oe"  #'eshell
    "of"  #'make-frame
    "oF"  #'select-frame-by-name
    "om"  #'mu4e
    "oo"  #'yejun/macos-reveal-in-finder

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
    "sg"  #'yejun/google-translate-guess-source-lang
    "sG"  #'yejun/google-translate-guess-source-lang-force-select
    "si"  #'consult-imenu
    "sI"  #'consult-imenu-multi
    "sk"  #'dash-at-point
    "sK"  #'dash-at-point-with-docset
    "sl"  #'ffap-menu
    "so"  #'yejun/lookup-online
    "ss"  #'yejun/search-buffer
    "sS"  #'yejun/search-buffer-for-symbol-at-point
    "sr"  #'yejun/search-project
    "sR"  #'yejun/search-project-for-symbol-at-point
    "st"  #'osx-dictionary-search-word-at-point

    "t"   '(:ignore t :which-key "toggle")
    "tf"  '(flymake-mode :which-key "Flymake mode")
    "tl"  '(logos-focus-mode :which-key "Logos-Focus Mode")
    "tr"  '(repeat-mode :which-key "Repeat mode")
    "tv"  '(visible-mode :which-key "Visible mode")

    "w"   '(evil-window-map :which-key "window")
    "wu"  #'winner-undo
    "wr"  #'winner-redo)

  (yejun/local-leader-key
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
    "e"  #'org-export-dispatch

    "c"  '(:ignore t :which-key "clock")
    "ci" #'org-clock-in

    "d"  '(:ignore t :which-key "date/deadline")
    "dd" #'org-deadline
    "ds" #'org-schedule
    "dt" #'org-time-stamp
    "dT" #'org-time-stamp-inactive

    "g"  '(:ignore t :which-key "goto")
    "gg" #'consult-org-heading
    "gG" #'consult-org-agenda
    "gr" #'org-refile-goto-last-stored
    "gx" #'org-capture-goto-last-stored

    "l"  '(:ignore t :which-key "links")
    "ll" #'org-insert-link
    "lL" #'org-insert-all-links
    "ls" #'org-store-link
    "lS" #'org-insert-last-stored-link
    "lt" #'org-toggle-link-display

    "o"  #'org-set-property
    "p"  #'org-priority

    "r"  '(:ignore t :which-key "refile")
    "r." #'+org/refile-to-current-file
    "rf" #'+org/refile-to-file
    "rr" #'org-refile
    "rR" #'org-refile-reverse

    "t"  #'org-todo)
  )

(defun yejun/browse-emacs-config ()
  (interactive)
  (yejun/find-file-in-project "~/.config/emacs"))

(defun yejun/browse-nix-config ()
  (interactive)
  (yejun/find-file-in-project "~/.config/nix-config/"))

(defun yejun/browse-blog ()
  (interactive)
  (yejun/find-file-in-project "~/src/yejun.dev"))

(provide 'init-evil-keybindings)
