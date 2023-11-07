(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq package-enable-at-startup nil)
(package-initialize)

(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq use-package-always-pin "gnu")

(provide 'init-elpa)
