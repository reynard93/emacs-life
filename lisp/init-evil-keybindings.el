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
    "a"   #'embark-act

    "p"   project-prefix-map
    "h"   help-map

    "'"   #'vertico-repeat
    "~"   #'popwin:popup-last-buffer
    ","   #'popwin:messages
    "/"   #'yejun/search-current-project
    "*"   #'yejun/search-current-project-for-symbol-at-point
    "`"   #'evil-switch-to-windows-last-buffer
    ";"   #'pp-eval-expression

    "SPC" #'project-switch-to-buffer
    "RET" #'bookmark-jump
    "x"   #'yejun/popup-scratch-buffer
    "X"   #'org-capture

    "b"   '(:ignore t :which-key "buffer")
    "bb"  #'switch-to-buffer
    "bB"  #'switch-to-buffer-other-window
    "bd"  #'dired-jump
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
    "fy"  #'yejun/yank-buffer-path
    "fY"  #'yejun/yank-buffer-path-relative-to-project

    "g"   '(:ignore t :which-key "git")
    "g'"  #'forge-dispatch
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
    "nc"  #'yejun/toggle-last-clock
    "nC"  #'org-clock-cancel
    "nd"  #'denote-date
    "nD"  #'denote-subdirectory
    "nf"  #'yejun/browse-notes
    "nF"  #'yejun/browse-org
    "nl"  #'denote-link
    "nn"  #'denote
    "nN"  #'denote-type
    "no"  #'org-clock-goto
    "ns"  #'yejun/search-notes
    "nS"  #'yejun/search-notes-for-symbol-at-point
    "nt"  #'denote-template

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
    "sk"  #'dash-at-point
    "sK"  #'dash-at-point-with-docset
    "sl"  #'ffap-menu
    "ss"  #'yejun/search-buffer
    "sS"  #'yejun/search-buffer-for-symbol-at-point
    "sr"  #'yejun/search-current-project
    "sR"  #'yejun/search-current-project-for-symbol-at-point
    "st"  #'osx-dictionary-search-word-at-point

    "t"   '(:ignore t :which-key "toggle")
    "tf"  '(flymake-mode :which-key "Flymake")
    "tn"  '(yejun/toggle-nix-formatter :which-key "Nix formatter")
    "tr"  '(read-only-mode :which-key "Read-only mode")
    "tv"  '(visible-mode :which-key "Visible mode")

    "w"   '(evil-window-map :which-key "window")
    "wu"  #'winner-undo
    "wr"  #'winner-redo)

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
    "e"  #'org-export-dispatch

    "c"  '(:ignore t :which-key "clock")
    "ci" #'org-clock-in)
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

(defun yejun/show-current-file-in-finder ()
  (interactive)
  (shell-command (concat "open -R " (shell-quote-argument (buffer-file-name)))))

(defun yejun/popup-scratch-buffer ()
  (interactive)
  (pop-to-buffer "*scratch*"))

(defun yejun/yank-buffer-path ()
  (interactive)
  (if-let ((filename (buffer-file-name)))
      (progn
        (kill-new filename)
        (message "Copied path: %s" filename))
    (error "Couldn't find filename in current buffer")))

(defun yejun/yank-buffer-path-relative-to-project ()
  (interactive)
  (if-let* ((filename (buffer-file-name))
            (path (file-relative-name filename (yejun/current-project-root))))
      (progn
        (kill-new path)
        (message "Copied path: %s" path))
    (error "Couldn't find filename in current buffer")))

(provide 'init-evil-keybindings)
