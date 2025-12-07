;;; -*- lexical-binding: t -*-

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Fix for native compilation on macOS with MacPorts libgccjit
(setenv "PATH" (concat "/opt/homebrew/bin:/opt/local/bin:" (getenv "PATH")))
(add-to-list 'exec-path "/opt/homebrew/bin")
(setenv "SDKROOT" (shell-command-to-string "xcrun --show-sdk-path | tr -d '\n'"))
(setq native-comp-driver-options '("-B/opt/local/bin" "-I/opt/local/include/gcc14" "-L/opt/local/lib/gcc14"))

(setq native-comp-async-report-warnings-errors 'silent)

;; Boost startup performance
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs started in %s" (emacs-init-time))
            ))
(setq package-enable-at-startup nil)

(provide 'early-init)
