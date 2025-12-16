;;; -*- lexical-binding: t -*-

(use-package agent-shell-manager
  :ensure (:host github :repo "reynard93/agent-shell-manager" :branch "fix/emacs31-refresh-crash")
  :after agent-shell
  :bind (("C-c A m" . agent-shell-manager-toggle)))

(provide 'init-agent-shell-manager)
