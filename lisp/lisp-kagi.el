(defun +kagi-token ()
  (auth-source-pass-get 'secret "kagi.com/token"))

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
  (let* ((token (+kagi-token))
         (query (+kagi-query))
         (query (+kagi-query-prompt query bang))
         (formatted-query (if bang (format "%s %s" bang query) query)))
    (browse-url (format "https://kagi.com/search?token=%s&q=%s" token (url-hexify-string formatted-query)))))

;; https://help.kagi.com/kagi/features/bangs.html#ai-related-bangs
(defun +kagi/summarize-url (url)
  "Summerize URL using Kagi's Universal Summarizer."
  (interactive "sEnter URL: ")
  (let ((token (+kagi-token)))
    (browse-url (format "https://kagi.com/search?token=%s&q=!sum %s" token (url-hexify-string url)))))

(with-eval-after-load 'embark
  (keymap-set embark-url-map "K" #'+kagi/summarize-url))

;; https://help.kagi.com/kagi/features/bangs.html#kagi-assistant-bangs
(defun +kagi/assistant-research ()
  (interactive)
  (+kagi/search "!expert"))

(defun +kagi/assistant-code ()
  (interactive)
  (+kagi/search "!code"))

(defun +kagi/assistant-chat ()
  (interactive)
  (+kagi/search "!chat"))

(defun +kagi/assistant-custom (&optional instruction)
  (interactive)
  (let ((search-type (if instruction
                         (concat "!custom" instruction)
                       "!custom")))
    (+kagi/search search-type)))

(defvar-keymap kagi-keymap
  "k" #'+kagi/search
  "r" #'+kagi/assistant-research
  "d" #'+kagi/assistant-code
  "c" #'+kagi/assistant-chat
  "a" #'+kagi/assistant-custom)

(bind-key "C-c k" kagi-keymap)

(provide 'lisp-kagi)
