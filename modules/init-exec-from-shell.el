;;; -*- lexical-binding: t -*-

;; Use exec-path-from-shell to inherit shell environment in GUI Emacs
;; This is especially important on macOS where GUI apps don't inherit shell env
;; Note: mise.el handles per-buffer environment for version-managed tools
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :init
  ;; Only initialize once
  (unless (bound-and-true-p exec-path-from-shell--initialized)
    (exec-path-from-shell-initialize)
    (setq exec-path-from-shell--initialized t)
    ;; Add emacs bin to PATH if not already there
    (let ((emacs-bin-path (expand-file-name "bin" user-emacs-directory)))
      (unless (member emacs-bin-path exec-path)
        (setq exec-path (cons emacs-bin-path exec-path))
        (setenv "PATH" (concat emacs-bin-path path-separator (getenv "PATH"))))))
  :custom
  ;; Update PATH and other important environment variables from shell
  ;; mise.el will override these per-buffer as needed
  (exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH" "GOPRIVATE")))

(provide 'init-exec-from-shell)
