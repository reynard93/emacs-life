(use-package gptel
  :pin melpa
  :init
  (defvar gptel--openai nil
    "Override the variable to hide ChatGPT models")

  (defvar gptel--openrouter
    (gptel-make-openai "OpenRouter"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :stream t
      :key (lambda () (auth-source-pass-get 'secret "api-key/openrouter"))
      :models '("anthropic/claude-3-haiku"
                "anthropic/claude-3-opus"
                "anthropic/claude-3.5-sonnet"
                "openai/gpt-4o"
                "openai/gpt-4o-mini")))

  (defvar gptel--kagi
    (gptel-make-kagi "Kagi"
      :key (lambda () (auth-source-pass-get 'secret "api-key/kagi"))
      :models '(fastgpt)))

  (defvar gptel--google
    (gptel-make-gemini "Google"
      :key (lambda () (auth-source-pass-get 'secret "api-key/gemini"))
      :stream t))

  :bind
  (("C-c <return>" . gptel-send)
   ("C-c C-<return>" . gptel-menu)
   ("C-c M-<return>" . +gptel/send-all-buffers))

  :custom
  (gptel-default-mode 'org-mode)
  (gptel-backend gptel--openrouter)
  (gptel-model "anthropic/claude-3.5-sonnet")

  :config
  (defun +gptel/send-all-buffers (text)
    "Send TEXT to all buffers where gptel-mode is active and execute `gpt-send'."
    (interactive "sEnter text: ")
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (bound-and-true-p gptel-mode)
          (save-excursion
            (goto-char (point-max))
            (insert text)
            (gptel-send))))))

  (defun +gptel/kagi-summarize-url (url)
    "Summarize URL using Kagi's Universal Summarizer."
    (interactive "sSummarize URL: ")
    (let ((gptel-backend gptel--kagi)
          (gptel-model 'summarize:agnes))
      (gptel-request url
        :callback
        (lambda (response info)
          (if response
              (let ((output-name (format "%s (summary)" (plist-get (plist-get info :data) :url))))
                (with-current-buffer (get-buffer-create output-name)
                  (let ((inhibit-read-only t))
                    (erase-buffer)
                    (visual-line-mode 1)
                    (insert response)
                    (display-buffer (current-buffer))
                    (special-mode))))
            (message "gptel-request failed with message: %s"
                     (plist-get info :status)))))
      (message "Generating summary for: %s" url))))

(use-package gptel-quick
  :vc (gptel-quick :url "https://github.com/karthink/gptel-quick.git")
  :after embark
  :config
  (setq gptel-quick-backend gptel--google
        gptel-quick-model 'gemini-1.5-flash)
  :bind (:map embark-general-map ("?" . gptel-quick)))

(provide 'init-gpt)
