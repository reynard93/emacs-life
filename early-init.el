;;; -*- lexical-binding: t -*-

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Fix for native compilation on macOS with MacPorts libgccjit
;; PATH is now handled by exec-path-from-shell in init-bootstrap.el
(setenv "SDKROOT" (shell-command-to-string "xcrun --show-sdk-path | tr -d '\n'"))
(setq native-comp-driver-options '("-B/opt/local/bin" "-I/opt/local/include/gcc14" "-L/opt/local/lib/gcc14"))

(setq native-comp-async-report-warnings-errors 'silent)

;; Boost startup performance by deferring garbage collection during init
;; This is reset after startup in the hook below
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs started in %s" (emacs-init-time))
            ;; Reset GC to reasonable values after startup
            ;; These values are defined in init-performance.el
            (setq gc-cons-threshold (* 1024 1024 50)  ; 50MB
                  gc-cons-percentage 0.2)))
(setq package-enable-at-startup nil)

(provide 'early-init)
