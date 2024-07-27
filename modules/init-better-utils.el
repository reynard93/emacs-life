(use-package undo-fu-session
  :pin melpa
  :preface
  (setq undo-limit (* 64 1024 1024)
        undo-strong-limit (* 96 1024 1024)
        undo-outer-limit (* 960 1024 1024))
  :hook (prog-mode text-mode conf-mode))

(use-package helpful
  :pin melpa
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)))

(use-package crux
  :pin melpa
  :bind (([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c M-d" . crux-duplicate-and-comment-current-line-or-region)
         ("s-k" . crux-smart-kill-line)
         ("s-j" . crux-top-join-line)
         ("C-^" . crux-switch-to-previous-buffer)
         ("s-n" . crux-create-scratch-buffer)))

(provide 'init-better-utils)
