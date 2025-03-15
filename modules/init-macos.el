(use-package macos
  :ensure nil
  :load-path "site-lisp/"
  :if (eq system-type 'darwin)
  :demand t
  :bind
  (("s-o" . find-file)
   ("s-s" . save-buffer)
   ("s-S" . write-file)
   ("s-a" . mark-whole-buffer)
   ("s-c" . kill-ring-save)
   ("s-v" . yank)
   ("s-x" . kill-region)
   ("s-z" . undo)
   ("s-Z" . undo-redo))
  :custom
  (vscode-program "cursor"))

(use-package alfred
  :ensure nil
  :load-path "site-lisp/"
  :if (eq system-type 'darwin)
  :bind
  (:map my-insert-map
   ("b m" . alfred-browser-md-link)
   ("b o" . alfred-browser-org-link)
   ("b t" . alfred-browser-title)
   ("b u" . alfred-browser-url)))

(when (or sys/mac-ns-p sys/mac-port-p)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-hook 'server-after-make-frame-hook
            (lambda ()
              (if (display-graphic-p)
                  (menu-bar-mode 1)
                (menu-bar-mode -1))))

  (defun refresh-ns-appearance ()
    "Refresh frame parameter ns-appearance."
    (let ((bg (frame-parameter nil 'background-mode)))
      (set-frame-parameter nil 'ns-appearance bg)
      (setcdr (assq 'ns-appearance default-frame-alist) bg)))
  (add-hook 'after-load-theme-hook #'refresh-ns-appearance)
  (with-eval-after-load'auto-dark
   (add-hook 'auto-dark-dark-mode-hook #'refresh-ns-appearance)
   (add-hook 'auto-dark-light-mode-hook #'refresh-ns-appearance)))

(provide 'init-macos)
