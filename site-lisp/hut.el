;;; hut.el -- Code for hut, the sr.ht CLI -*- lexical-binding: t -*-

;;; Commentary:
;;
;; https://git.sr.ht/~xenrox/hut

;;; Code:

(defun hut-paste-create ()
  (interactive)
  (let ((filename (buffer-name))
        (output-buffer "*paste-output*"))
    (shell-command-on-region
     (if (use-region-p) (region-beginning) (point-min))
     (if (use-region-p) (region-end) (point-max))
     (format "hut paste create --name %s" filename)
     output-buffer)
    (with-current-buffer output-buffer
      (goto-char (point-max))
      (forward-line -1)
      (kill-new (thing-at-point 'url)))
    (kill-buffer output-buffer)))

(provide 'hut)
