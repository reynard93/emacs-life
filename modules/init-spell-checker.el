;;; -*- lexical-binding: t -*-

;; prb using this, bcz emacs-plus intel but its looking at arm
;; https://github.com/minad/jinx/issues/134
(use-package jinx
  :disabled
  :ensure nil
  :hook org-mode
  :bind
  (([remap ispell-word] . jinx-correct)
   ("C-c J" . jinx-languages)
   ("<f12>" . jinx-mode))
  :custom
  (jinx-languages "en_US en_GB"))

(provide 'init-spell-checker)
