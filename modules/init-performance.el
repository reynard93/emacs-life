;; Performance optimizations for Emacs

;; Increase garbage collection threshold for better overall performance
(setq gc-cons-threshold (* 1024 1024 50)) ; 50MB (default is 800KB)

;; Only collect garbage when idle
(defvar my/gc-timer nil
  "Timer for garbage collection.")

(defun my/garbage-collect-when-idle ()
  "Run garbage collection when Emacs is idle."
  (when my/gc-timer
    (cancel-timer my/gc-timer))
  (setq my/gc-timer
        (run-with-idle-timer 5 nil
                            (lambda ()
                              (message "Garbage collecting...")
                              (garbage-collect)
                              (message "Garbage collecting...done")))))

(add-hook 'focus-out-hook #'my/garbage-collect-when-idle)

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
