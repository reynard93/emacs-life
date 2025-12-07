;;; -*- lexical-binding: t -*-

;; Making custom-file disposable
(setq custom-file (make-temp-file "emacs-custom-"))

(require 'init-elpaca)

;; Set $PATH correctly
;; other ways: https://www.reddit.com/r/emacs/comments/1fxha3a/homebrew_emacs_plus_path_issue_and_solutions_in/
;; Enabling `envrc-global-mode'
(use-package envrc
  :hook (after-init . envrc-global-mode))

(provide 'init-bootstrap)
