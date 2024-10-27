;;; fabric-ai.el -- Code for fabric -*- lexical-binding: t -*-

;;; Commentary:
;;
;; fabric is an open-source framework for augmenting humans using AI.
;; https://github.com/danielmiessler/fabric

;;; Code:

(defun fabric-ai-summarize-url (url)
  "Summarize URL using fabric."
  (interactive "sURL: ")
  (let ((response (shell-command-to-string
                   (format "fabric -p summarize -u %s"
                           (shell-quote-argument url))))
        (buffer (get-buffer-create
                 (format "%s (summary)" url))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (visual-line-mode 1)
        (insert response)
        (display-buffer
         (current-buffer)
         '((display-buffer-in-side-window)
           (side . right)
           (window-width . 0.5)))
        (special-mode)))))

(provide 'fabric-ai)
;;; fabric-ai.el ends here
