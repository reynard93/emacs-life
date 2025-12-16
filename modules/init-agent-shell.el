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
  :bind (("C-c A s" . my/agent-shell-new-default)
         ("C-c A c" . agent-shell-anthropic-start-claude-code)
         ("C-c A p" . my/agent-shell-opencode-copilot))
  :config
  ;; Claude: login-based auth (subscription) + inherit PATH.
  (require 'agent-shell-anthropic)
  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :login t)
        agent-shell-anthropic-claude-environment
        (agent-shell-make-environment-variables :inherit-env t))

  (require 'agent-shell-opencode)

  ;; Default OpenCode: uses OPENCODE_API_KEY (OpenCode Zen).
  (setq agent-shell-opencode-authentication
        (agent-shell-opencode-make-authentication
         :api-key (lambda ()
                   (or (getenv "OPENCODE_API_KEY")
                       (user-error "Missing OPENCODE_API_KEY"))))
        agent-shell-opencode-environment
        (agent-shell-make-environment-variables :inherit-env t))

  (defun my/agent-shell-opencode-copilot--make-agent-config ()
    "OpenCode agent configured for GitHub Copilot oauth + gpt-5.2."
    (agent-shell-make-agent-config
     :mode-line-name "OpenCode Copilot"
     :buffer-name "OpenCode Copilot"
     :shell-prompt "OpenCode Copilot> "
     :shell-prompt-regexp "OpenCode Copilot> "
     :welcome-function #'agent-shell-opencode--welcome-message
     :client-maker (lambda (buffer)
                    (let ((agent-shell-opencode-authentication
                           (agent-shell-opencode-make-authentication :none t))
                          (agent-shell-opencode-command
                           '("opencode" "--model" "github-copilot/gpt-5.2" "acp"))
                          (agent-shell-opencode-environment
                           (agent-shell-make-environment-variables :inherit-env t)))
                      (agent-shell-opencode-make-client :buffer buffer)))
     :install-instructions "See https://opencode.ai/docs for installation."))

  (defun my/agent-shell-new-default ()
    "Start a NEW agent-shell using `agent-shell-preferred-agent-config'."
    (interactive)
    (agent-shell t))

  (defun my/agent-shell-opencode-copilot ()
    "Start a NEW OpenCode (GitHub Copilot gpt-5.2) agent shell."
    (interactive)
    (agent-shell--dwim :config (my/agent-shell-opencode-copilot--make-agent-config)
                       :new-shell t))

  ;; Make OpenCode (Zen / OPENCODE_API_KEY) the default for `M-x agent-shell`.
  (setq agent-shell-preferred-agent-config
        (agent-shell-opencode-make-agent-config)))

(provide 'init-agent-shell)
