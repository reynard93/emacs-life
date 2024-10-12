(use-package gptel
  :pin melpa
  :init
  (defvar gptel--openai nil
    "Override the variable to hide ChatGPT models")

  (defvar gptel--azure
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
                "openai/gpt-4o-mini"
                "openai/gpt-4o"
                "deepseek/deepseek-chat")))

  (defvar gptel--kagi
    (gptel-make-kagi "Kagi"
      :key (lambda () (auth-source-pass-get 'secret "api-key/kagi"))
      :models '("fastgpt")))

  :bind
  (("C-c <return>" . gptel-send)
   ("C-c C-<return>" . gptel-menu)
   ("C-c M-<return>" . +gptel/send-all-buffers))

  :custom
  (gptel-default-mode 'org-mode)
  (gptel-model "gpt-4o")

  :config
  (setq gptel-backend gptel--azure)

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
      (message "Generating summary for: %s" url))))

(use-package gptel-quick
  :vc (gptel-quick :url "https://github.com/karthink/gptel-quick.git")
  :after embark
  :config
  (setq gptel-quick-backend gptel--openrouter
        gptel-quick-model "anthropic/claude-3-haiku")
  :bind (:map embark-general-map ("?" . gptel-quick)))

(provide 'init-gpt)
