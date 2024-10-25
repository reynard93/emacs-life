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
   ("s-Z" . undo-redo)
   :map embark-url-map
   ("U" . macos-read-it-later)))

(use-package exec-path-from-shell
  :pin nongnu
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize)
  ;; Add Homebrew's executable files to PATH.
  (let ((homebrew-bin-dir "/opt/homebrew/bin"))
    (when (and (file-directory-p homebrew-bin-dir)
               (not (string-match-p homebrew-bin-dir (getenv "PATH"))))
      (setenv "PATH" (concat (getenv "PATH") ":" homebrew-bin-dir)))))

(provide 'init-macos)
