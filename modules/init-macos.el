;;; -*- lexical-binding: t -*-

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

(provide 'init-macos)
