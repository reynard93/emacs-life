;; Making custom-file disposable
(setq custom-file (make-temp-file "emacs-custom-"))

;; Initializing package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq use-package-always-pin "gnu")

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

;; Set $PATH correctly
(use-package exec-path-from-shell
  :pin nongnu
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize)
  ;; Add Homebrew's executable files to PATH.
  (let ((homebrew-bin-dir "/opt/homebrew/bin"))
    (when (and (file-directory-p homebrew-bin-dir)
               (not (string-match-p homebrew-bin-dir (getenv "PATH"))))
      (setenv "PATH" (concat (getenv "PATH") ":" homebrew-bin-dir)))))

;; Enabling `envrc-global-mode'
(use-package envrc
  :pin melpa
  :hook (after-init . envrc-global-mode))

;; Enabling Emacs server
(use-package server
  :ensure nil
  :if (display-graphic-p)
  :defer 20
  :custom
  (server-name "gui")
  :config
  (unless (server-running-p)
    (server-start)))

(provide 'init-bootstrap)
