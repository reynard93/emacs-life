(defun +kagi--token ()
  (auth-source-pass-get 'secret "kagi.com/token"))

(defun +kagi--query ()
  (let ((selected-text (if (use-region-p)
                           (buffer-substring-no-properties (region-beginning) (region-end))
                         (or (thing-at-point 'symbol t) ""))))
    (string-trim selected-text)))

(defun +kagi/search (&optional assistant-type)
  (interactive)
  (let* ((token (+kagi--token))
         (query (+kagi--query))
         (formatted-query (if assistant-type (format "%s %s" assistant-type query) query)))
    (browse-url (format "https://kagi.com/search?token=%s&q=%s" token (url-hexify-string formatted-query)))))

(defun +kagi/assistant-research (&optional arg)
  (interactive "P")
  (if arg
      (+kagi/search "!expert")
    (+kagi/search "!fast")))

(defun +kagi/assistant-code ()
  (interactive)
  (+kagi/search "!code"))

(defun +kagi/assistant-chat ()
  (interactive)
  (+kagi/search "!chat"))

(defun +kagi/assistant-custom (&optional bang)
  (interactive)
  (let ((search-type (format "!custom %s" bang)))
    (+kagi/search search-type)))

(defun +kagi/assistant-custom-translate ()
  (interactive)
  (+kagi/assistant-custom "!t"))

(defun +kagi/assistant-custom-localize ()
  (interactive)
  (+kagi/assistant-custom "!l"))

(provide 'lisp-kagi)
