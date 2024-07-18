(defun +kagi-query ()
  (let ((selected-text (if (use-region-p)
                           (buffer-substring-no-properties (region-beginning) (region-end))
                         (or (thing-at-point 'symbol t) ""))))
    (string-trim selected-text)))

(defun +kagi-query-prompt (query &optional bang)
  (let* ((initial-contents (if bang (concat "\n" query) query))
         (prompt (if bang (format "Kagi Search [%s]: " bang) "Kagi Search: ")))
    (let* ((minibuffer-setup-hook (lambda () (goto-char (minibuffer-prompt-end)))))
      (read-from-minibuffer prompt initial-contents))))

(defun +kagi/search (&optional bang)
  (interactive)
  (let* ((query (+kagi-query))
         (query (+kagi-query-prompt query bang))
         (formatted-query (if bang (format "%s %s" bang query) query)))
    (browse-url (format "https://kagi.com/search?q=%s" (url-hexify-string formatted-query)))))

(bind-key "s" #'+kagi/search search-map)

(provide 'lisp-kagi)
