;;; -*- lexical-binding: t -*-

(use-package csv-mode
  :defer t)

(use-package json-mode
  :defer t)

(use-package sqlite-mode
  :ensure nil
  :defer t)

(use-package pg
  :ensure (:host github :repo "emarsden/pg-el"))

;; see https://github.com/emarsden/pgmacs/issues/9
;; there is a bug right now cannot load the tables, when
;; use M-x load-file RET /Users/reynardtw/.emacs.d/elpaca/repos/pg-el.github.emarsden/pg.el
;; then M-x pgmacs-open-uri postgresql://root:secret@localhost:5432/formflow_development
;; then it works
(use-package pgmacs
  :after pg
  :ensure (:host github :repo "emarsden/pgmacs"))

;; seems useful put under utils
(defun copy-buffer-file-path ()
  "Copy the current buffer's file path to the kill ring (clipboard)."
  (interactive)
  (let ((filepath (buffer-file-name)))
    (if filepath
        (progn
          (kill-new filepath)
          (message "Copied to clipboard: %s" filepath))
      (message "Buffer is not visiting a file"))))

(provide 'init-data)
