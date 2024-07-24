(use-package gptel
  :pin melpa
  :init
  (defvar gptel--openai
    (gptel-make-azure "Azure"
      :host "beepboop.openai.azure.com"
      :endpoint "/openai/deployments/gpt-4o/chat/completions?api-version=2024-06-01"
      :stream t
      :key (lambda () (auth-source-pass-get 'secret "api-key/beepboop"))
      :models '("gpt-4o")))

  (defvar gptel--openrouter
    (gptel-make-openai "OpenRouter"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :stream t
      :key (lambda () (auth-source-pass-get 'secret "api-key/openrouter"))
      :models '("anthropic/claude-3.5-sonnet"
                "anthropic/claude-3-haiku"
                "anthropic/claude-3-opus"
                "google/gemini-flash-1.5"
                "google/gemini-pro-1.5"
                "openai/gpt-4o-mini"
                "openai/gpt-4o")))

  (defvar gptel--groq
    (gptel-make-openai "Groq"
      :host "api.groq.com"
      :endpoint "/openai/v1/chat/completions"
      :stream t
      :key (lambda () (auth-source-pass-get 'secret "api-key/groq"))
      :models '("mixtral-8x7b-32768"
                "llama3-70b-8192"
                "llama3-8b-8192"
                "gemma2-9b-it"
                "gemma-7b-it")))

  (defvar gptel--kagi
    (gptel-make-kagi "Kagi"
      :key (lambda () (auth-source-pass-get 'secret "api-key/kagi"))
      :models nil))

  (setq-default gptel-backend gptel--openai
                gptel-model "gpt-4o")

  :config
  (message "gptel is loaded")

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

  (defun +gptel/send (&optional arg)
    (interactive "P")
    (cond
     (gptel-mode (gptel-send arg))
     ((use-region-p) (gptel-send arg))
     ((< (point) 2000) (gptel-send arg))
     ((y-or-n-p "Prompt has more than 2000 chars, really send?") (gptel-send arg))
     (t (message "Request cancelled"))))

  (defun +gptel/kagi-summarize-url (url)
    "Summarize URL using Kagi's Universal Summarizer."
    (interactive "sSummarize URL: ")
    (let ((gptel-backend gptel--kagi)
          (gptel-model "summarize:agnes"))
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
      (message "Generating summary for: %s" url)))

  :custom
  (gptel-max-tokens 1000)
  (gptel-default-mode 'org-mode)
  :bind (("C-c C-<return>" . gptel-menu)
         ("C-c <return>" . +gptel/send)
         :map gptel-mode-map
         ("C-c C-x t" . gptel-set-topic)
         ("M-n" . gptel-end-of-response)
         :map embark-url-map
         ("?" . +gptel/kagi-summarize-url)))

(use-package gptel-quick
  :vc (gptel-quick :url "https://github.com/karthink/gptel-quick.git")
  :config
  (message "gptel-quick is loaded")
  (setq gptel-quick-backend gptel--openrouter
        gptel-quick-model "openai/gpt-4o-mini")
  :bind ( :map embark-general-map
          ("?" . gptel-quick)))

(provide 'init-gpt)
