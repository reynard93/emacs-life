;;; -*- lexical-binding: t -*-

;; Use exec-from-shell to properly integrate with fish shell
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :init
  ;; Only initialize once
  (unless (bound-and-true-p exec-path-from-shell--initialized)
    (exec-path-from-shell-initialize)
    (setq exec-path-from-shell--initialized t)
    ;; Add ruby-lsp and emacs bin to PATH if not already there
    (let ((ruby-lsp-path "/opt/homebrew/share/gems/bin")
          (emacs-bin-path (expand-file-name "bin" user-emacs-directory)))
      (unless (member ruby-lsp-path exec-path)
        (setq exec-path (cons ruby-lsp-path exec-path))
        (setenv "PATH" (concat ruby-lsp-path path-separator (getenv "PATH"))))
      (unless (member emacs-bin-path exec-path)
        (setq exec-path (cons emacs-bin-path exec-path))
        (setenv "PATH" (concat emacs-bin-path path-separator (getenv "PATH"))))))
  :custom
  ;; Update PATH and other important environment variables
  (exec-path-from-shell-variables '("PATH" "MANPATH" "GOPATH" "GOPRIVATE" "GEM_HOME" "GEM_PATH" "NODE_PATH" "PYTHONPATH")))

(provide 'init-exec-from-shell)
