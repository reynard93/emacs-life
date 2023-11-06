(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(require 'use-package-ensure)
(setq use-package-always-ensure t)
(setq use-package-always-pin "gnu")

(setq package-enable-at-startup nil)
(package-initialize)

(provide 'init-elpa)
