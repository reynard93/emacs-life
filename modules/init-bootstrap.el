;;; -*- lexical-binding: t -*-

;; Making custom-file disposable
(setq custom-file (make-temp-file "emacs-custom-"))

(require 'init-elpaca)

;; Initialize environment from shell
;; This sets the baseline PATH and environment variables from your shell
;; mise.el (below) will then override these per-buffer for project-specific versions
(use-package exec-path-from-shell
  :ensure t
  :demand t
  :custom
  ;; Copy these specific variables from shell environment
  (exec-path-from-shell-variables
   '("PATH" "MANPATH" "GOPATH" "CARGO_HOME" "GEM_HOME" "GEM_PATH" "LINEAR_API_KEY"))
  ;; Use a faster shell invocation
  (exec-path-from-shell-arguments '("-l"))
  :config
  (exec-path-from-shell-initialize))

;; Set $PATH correctly using mise.el for version manager integration
;; mise.el sets environment variables (PATH, GEM_HOME, etc.) per-buffer
;; based on mise configuration files (.mise.toml, .tool-versions)
;; https://github.com/eki3z/mise.el
(use-package mise
  :ensure (:host github :repo "liuyinz/mise.el")
  :demand t  ; Load immediately, not lazily
  :config
  ;; Ensure mise can find the executable
  (setq mise-executable (or (executable-find "mise")
                            "/opt/homebrew/bin/mise"))
  ;; Enable debug mode temporarily to diagnose issues
  ;; (setq mise-debug t)
  ;; Disable mise in org buffers
  (add-hook 'org-mode-hook (lambda () (mise-mode -1)))
  ;; Enable global mode immediately after config
  (global-mise-mode 1))

(provide 'init-bootstrap)
