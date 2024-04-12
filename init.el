;;; Making custom-file disposable
(setq custom-file (make-temp-file "emacs-custom-"))

;;; macOS-compatible keybindings
(when (display-graphic-p)
  (bind-key "s-x" #'execute-extended-command)  ; M-x
  (bind-key "s-s" #'save-buffer)               ; File -> Save
  (bind-key "s-S" #'write-file)                ; File -> Save As
  (bind-key "s-a" #'mark-whole-buffer)         ; File -> Select All
  (bind-key "s-c" #'kill-ring-save)            ; Edit -> Copy
  (bind-key "s-v" #'yank)                      ; Edit -> Paste
  (bind-key "s-z" #'undo))                     ; Edit -> Undo

;;; Load modules
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Bootstrap
(require 'init-lisp)
(require 'init-elpa)
(require 'init-env)

;; Interaction
(require 'init-minibuffer)
(require 'init-completion)
(require 'init-window)
(require 'init-evil)

;; Visualization
(require 'init-ui)
(require 'init-pulse)
(require 'init-mode-line)

;; Common
(require 'init-better-defaults)
(require 'init-editing-utils)
(require 'init-gui-frames)

;; Note-taking
(require 'init-denote)
(require 'init-org)

;; Programming
(require 'init-tree-sitter)
(require 'init-eglot)
(require 'init-data)
(require 'init-elixir)
(require 'init-markdown)
(require 'init-nix)
(require 'init-racket)
(require 'init-ruby)
(require 'init-web)
(require 'init-yaml)

;; Tools
(require 'init-git)
(require 'init-pass)
(require 'init-mail)
(require 'init-irc)
(require 'init-gpt)
(require 'init-timer)
(require 'init-lookup)
(require 'init-reading)
(require 'init-writing)
(require 'init-restclient)
(require 'init-network-tools)
(require 'init-spell-checker)

;; Keybindings
(require 'init-emacs-keybindings)
(require 'init-evil-keybindings)

;; Misc
(require 'init-project)
(require 'init-misc)
