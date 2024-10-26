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
        (special-mode)
        (display-buffer buffer)))))

(provide 'fabric-ai)
