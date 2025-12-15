;;; -*- lexical-binding: t -*-

;; Pin explicit recipes so Elpaca doesn't need MELPA metadata.
(use-package shell-maker
  :ensure (:host github :repo "xenodium/shell-maker"))

(use-package acp
  :ensure (:host github :repo "xenodium/acp.el"))

(use-package agent-shell
  :ensure (:host github :repo "xenodium/agent-shell")
  :after (acp shell-maker)
  ;; Avoid clobbering `org-agenda' on C-c a.
  :bind (("C-c A s" . agent-shell))
  :config
  ;; Claude: login-based auth (subscription) + inherit PATH.
  (require 'agent-shell-anthropic)
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t)
        agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables :inherit-env t))

  ;; OpenCode: uses OPENCODE_API_KEY.
  (require 'agent-shell-opencode)
  (setq agent-shell-opencode-authentication
        (agent-shell-opencode-make-authentication
         :api-key (lambda ()
                   (or (getenv "OPENCODE_API_KEY")
                       (user-error "Missing OPENCODE_API_KEY"))))
        agent-shell-opencode-environment
        (agent-shell-make-environment-variables :inherit-env t)
        ;; Make OpenCode the default for `M-x agent-shell`.
        agent-shell-preferred-agent-config
        (agent-shell-opencode-make-agent-config)))

(provide 'init-agent-shell)
