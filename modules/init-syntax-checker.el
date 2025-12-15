;;; -*- lexical-binding: t -*-

(use-package flycheck
  :hook ((prog-mode ruby-ts-mode) . flycheck-mode)
  :config
  ;; Ensure flycheck re-verifies executables when buffer environment changes
  ;; This is important for mise.el integration where exec-path is buffer-local
  (add-hook 'mise-mode-hook
            (lambda ()
              (when (bound-and-true-p flycheck-mode)
                (flycheck-buffer))))

  ;; Explicitly disable flymake when flycheck is enabled
  (add-hook 'flycheck-mode-hook
            (lambda ()
              (when (bound-and-true-p flymake-mode)
                (flymake-mode -1)))))

(use-package flyover
  :ensure (:host github :repo "konrad1977/flyover")
  :hook (flycheck-mode . flyover-mode)
  :custom
  ;; Checker settings
  (flyover-checkers '(flycheck))
  (flyover-levels '(error warning info))

  ;; Appearance
  (flyover-use-theme-colors t)
  (flyover-background-lightness 45)
  (flyover-percent-darker 40)
  (flyover-text-tint 'lighter)
  (flyover-text-tint-percent 50)

  ;; Icons
  (flyover-info-icon "ⓘ")
  (flyover-warning-icon "⚠")
  (flyover-error-icon "✘")

  ;; Display settings
  (flyover-hide-checker-name t)
  (flyover-show-virtual-line t)
  (flyover-virtual-line-type 'curved-dotted-arrow)
  (flyover-line-position-offset 1)

  ;; Message wrapping
  (flyover-wrap-messages t)
  (flyover-max-line-length 80)

  ;; Performance
  (flyover-debounce-interval 0.2))

(use-package flycheck-jest
  :after flycheck
  )

(use-package consult-flycheck
  :after (consult flycheck))

(use-package flycheck-eglot
  :ensure t
  :after (flycheck eglot)
  :config
  (global-flycheck-eglot-mode 1)

  ;; Disable Flymake in Eglot-managed buffers to avoid duplicate diagnostics.
  ;; Flycheck-eglot bridges LSP diagnostics into Flycheck, so we don't need
  ;; Flymake's overlays which would otherwise duplicate Flycheck's display.
  (add-hook 'eglot-managed-mode-hook
            (lambda ()
              (when (bound-and-true-p flymake-mode)
                (flymake-mode -1)))))

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
