(defun +kagi-token ()
  (auth-source-pass-get 'secret "kagi.com/token"))

(defun +kagi-query ()
  (let ((selected-text (if (use-region-p)
                           (buffer-substring-no-properties (region-beginning) (region-end))
                         (or (thing-at-point 'symbol t) ""))))
    (string-trim selected-text)))

(defun +kagi-query-prompt (assistant-type)
  (let* ((query (+kagi-query))
         (initial-contents (concat "\n" query))
         (prompt (format "Kagi Assistant [%s]: " assistant-type)))
    (let* ((minibuffer-setup-hook (lambda () (goto-char (minibuffer-prompt-end)))))
      (read-from-minibuffer prompt initial-contents))))

(defun +kagi/search (&optional assistant-type)
  (interactive)
  (let* ((token (+kagi-token))
         (query (if assistant-type
                    (+kagi-query-prompt assistant-type)
                  (+kagi-query)))
         (formatted-query (if assistant-type (format "%s %s" assistant-type query) query)))
    (browse-url (format "https://kagi.com/search?token=%s&q=%s" token (url-hexify-string formatted-query)))))

(defun +kagi/assistant-research (&optional ask-expert)
  (interactive "P")
  (if ask-expert
      (+kagi/search "!expert")
    (+kagi/search "!fast")))

(defun +kagi/assistant-code ()
  (interactive)
  (+kagi/search "!code"))

(defun +kagi/assistant-chat ()
  (interactive)
  (+kagi/search "!chat"))

(defun +kagi/assistant-custom (&optional instruction)
  (interactive)
  (let ((search-type (format "!custom %s" instruction)))
    (+kagi/search search-type)))

(defun +kagi/assistant-custom-translate ()
  (interactive)
  (+kagi/assistant-custom "translate"))

(defun +kagi/assistant-custom-localize ()
  (interactive)
  (+kagi/assistant-custom "localize"))

(defvar kagi-assistant-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map "r" '+kagi/assistant-research)
    (define-key map "c" '+kagi/assistant-code)
    (define-key map "h" '+kagi/assistant-chat)
    (define-key map "a" '+kagi/assistant-custom)
    (define-key map "L" '+kagi/assistant-custom-localize)
    (define-key map "T" '+kagi/assistant-custom-translate)
    map)
  "Keymap for kagi assistant commands.")

(bind-key "C-c a" kagi-assistant-prefix-map)

(defun +kagi/summarize (url)
  "Summerize URL using Kagi's Universal Summarizer."
  (interactive "sSummerize URL: ")
  (browse-url (format "https://kagi.com/summarizer?summary=takeaway&url=%s" (url-hexify-string url))))

(with-eval-after-load 'embark
  (keymap-set embark-url-map "K" #'+kagi/summarize))

(provide 'lisp-kagi)
