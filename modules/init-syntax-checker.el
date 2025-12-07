;;; -*- lexical-binding: t -*-

(use-package flycheck
  :hook ((prog-mode ruby-ts-mode)
         . flycheck-mode))

(use-package flycheck-jest
  :after flycheck
  )

(use-package consult-flycheck
  :after (consult flycheck))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1))

(use-package plantuml-mode
  :config
  :disabled t
  (setq plantuml-executable-path (executable-find "plantuml"))
  (setq plantuml-jar-path (expand-file-name "../lib/plantuml.jar" (file-name-directory (file-truename plantuml-executable-path))))
  (setq org-plantuml-jar-path plantuml-jar-path)

  (setq plantuml-default-exec-mode 'executable)
  (setq plantuml-exec-mode 'executable)

  (setenv "PLANTUML_LIMIT_SIZE" "8192")

  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
  (add-to-list 'org-babel-load-languages '(plantuml . t))
  )

;; very good ruby config uses chruby and robocopfmt and inf-ruby
;; https://github.com/chadhs/dotfiles/blob/master/editors/emacs-config.org
(provide 'init-syntax-checker)
