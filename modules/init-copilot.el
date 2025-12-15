;;; -*- lexical-binding: t -*-

(use-package copilot
  :ensure (:host github :repo "copilot-emacs/copilot.el" :files ("*.el" "dist"))
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("C-<tab>" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion)
              ("C-M-<tab>" . 'copilot-accept-completion-by-word)
              ("C-M-TAB" . 'copilot-accept-completion-by-word))
  :config
  ;; Do not indent automatically on accept, just accept the completion
  (setq copilot-indent-offset-warning-disable t)

  ;; Configure major mode to language mapping
  (add-to-list 'copilot-major-mode-alist '("web-mode" . "html"))
  (add-to-list 'copilot-major-mode-alist '("typescript-ts-mode" . "typescript"))
  (add-to-list 'copilot-major-mode-alist '("tsx-ts-mode" . "typescriptreact"))
  (add-to-list 'copilot-major-mode-alist '("ruby-ts-mode" . "ruby"))
  (add-to-list 'copilot-major-mode-alist '("json-ts-mode" . "json"))
  (add-to-list 'copilot-major-mode-alist '("yaml-ts-mode" . "yaml"))
  (add-to-list 'copilot-major-mode-alist '("dockerfile-ts-mode" . "dockerfile")))

(provide 'init-copilot)
