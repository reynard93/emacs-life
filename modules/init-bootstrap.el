;; Making custom-file disposable
(setq custom-file (make-temp-file "emacs-custom-"))

;; Initializing package system
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq use-package-always-pin "gnu")

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))
(require 'vc-use-package)

;; Loading environment variables
(use-package exec-path-from-shell
  :pin nongnu
  :if (memq window-system '(mac ns))
  :config
  (exec-path-from-shell-initialize))

(provide 'init-bootstrap)
