;; Making custom-file disposable
(setq custom-file (make-temp-file "emacs-custom-"))

;; Initializing package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq use-package-always-pin "gnu")

;; Set $PATH correctly
;; other ways: https://www.reddit.com/r/emacs/comments/1fxha3a/homebrew_emacs_plus_path_issue_and_solutions_in/
;; causes too much slowdown for me but i hv to use
(use-package exec-path-from-shell
  :pin nongnu
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))

;; Enabling `envrc-global-mode'
(use-package envrc
  :pin melpa
  :hook (after-init . envrc-global-mode))

;; Disable for now, daemon is superior
;; Enabling Emacs server
;; (use-package server
;;   :ensure nil
;;   :if (display-graphic-p)
;;   :defer 20
;;   :custom
;;   (server-name "gui")
;;   :config
;;   (unless (server-running-p)
;;     (server-start)))

(provide 'init-bootstrap)
