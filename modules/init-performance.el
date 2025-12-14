;;; -*- lexical-binding: t -*-

;; Performance optimizations for Emacs

;; Increase garbage collection threshold for better overall performance
(setq gc-cons-threshold (* 1024 1024 50)
                  gc-cons-percentage 0.2)

(setq use-package-expand-minimally t)     ; Generate minimal code
(setq use-package-minimum-reported-time 0.1) ; Report packages that take >0.1s to load

;; Use gcmh (Garbage Collector Magic Hack) for better GC performance
(use-package gcmh
  :ensure t
  :demand t
  :config
  (setq gcmh-idle-delay 5
        gcmh-high-cons-threshold (* 50 1024 1024)) ; 50MB
  (gcmh-mode 1))

;; Increase read process output max (helps with LSP)
(setq read-process-output-max (* 1024 1024 3)) ; 3MB (default is 4KB)

;; Disable bidirectional text rendering for small performance boost
(setq-default bidi-display-reordering nil)

;; Disable expensive font-lock features
(setq font-lock-maximum-decoration t
      font-lock-maximum-size 256000
      font-lock-support-mode 'jit-lock-mode)

;; Use fundamental-mode for very large files
(defun my/check-large-file ()
  "Check if visiting a large file, if so use fundamental-mode."
  (when (> (buffer-size) (* 1024 1024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))

(add-hook 'find-file-hook #'my/check-large-file)

;; Faster scrolling
(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)

(provide 'init-performance)
