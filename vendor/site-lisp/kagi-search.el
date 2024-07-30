(defun kagi-search (&optional bang)
  (interactive)
  (let* ((query (kagi-search--query))
         (query (kagi-search--query-prompt query bang))
         (formatted-query (if bang (format "%s %s" bang query) query)))
    (browse-url (format "https://kagi.com/search?q=%s" (url-hexify-string formatted-query)))))

(defun kagi-search--query ()
  (let ((selected-text (if (use-region-p)
                           (buffer-substring-no-properties (region-beginning) (region-end))
                         (or (thing-at-point 'symbol t) ""))))
    (string-trim selected-text)))

(defun kagi-search--query-prompt (query &optional bang)
  (let* ((initial-contents (if bang (concat "\n" query) query)))
    (let* ((minibuffer-setup-hook (lambda () (goto-char (minibuffer-prompt-end)))))
      (read-from-minibuffer "Kagi search: " initial-contents))))

(provide 'kagi-search)
