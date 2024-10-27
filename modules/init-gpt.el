(use-package gptel
  :pin melpa
  :init
  (defvar gptel--openai nil
    "Override the variable to hide ChatGPT models")

  (defvar gptel--google
    (gptel-make-gemini "Google"
      :key (lambda () (auth-source-pass-get 'secret "api-key/gemini"))
      :stream t))

  (defvar gptel--kagi
    (gptel-make-kagi "Kagi"
      :key (lambda () (auth-source-pass-get 'secret "api-key/kagi"))
      :models '(fastgpt)))

  (defvar gptel--openrouter
    (gptel-make-openai "OpenRouter"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :stream t
      :key (lambda () (auth-source-pass-get 'secret "api-key/openrouter"))
      :models '(anthropic/claude-3-haiku
                anthropic/claude-3-opus
                anthropic/claude-3.5-sonnet
                openai/gpt-4o
                openai/gpt-4o-mini
                deepseek/deepseek-chat)))
  :bind
  (("C-c <return>" . gptel-send)
   ("C-c C-<return>" . gptel-menu)
   ("C-c M-<return>" . +gptel/send-all-buffers))

  :custom
  (gptel-default-mode 'org-mode)

  :config
  (setq gptel-model 'anthropic/claude-3.5-sonnet
        gptel-backend gptel--openrouter)

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

  (defun +gptel/summarize-url (url)
    "Summarize URL using Kagi.
Display the result in a side window."
    (interactive "sURL: ")
    (let ((gptel-backend gptel--kagi)
          (gptel-model 'summarize:agnes))
      (gptel-request url
        :callback
        (lambda (response info)
          (if response
              (with-current-buffer (get-buffer-create "*gptel-summary*")
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert response)
                  (display-buffer
                   (current-buffer)
                   `((display-buffer-in-side-window)
                     (side . bottom)
                     (window-height . ,#'fit-window-to-buffer))))
                (special-mode))
            (message "gptel-request failed with message: %s"
                     (plist-get info :status)))))))

  (defun +gptel/translate (text)
    "Translate TEXT into English using LLM.
If region is active, use it as TEXT; otherwise prompt for input.
Display the result in a side window with the content selected."
    (interactive
     (list (if (use-region-p)
               (buffer-substring-no-properties (region-beginning) (region-end))
             (read-string "Text: "))))
    (let ((gptel-backend gptel--openrouter)
          (gptel-model 'deepseek/deepseek-chat))
      (gptel-request text
        :system "You're a language translator. Translate text into English, response concisely."
        :callback
        (lambda (response info)
          (if response
              (with-current-buffer (get-buffer-create "*gptel-translate*")
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert response)
                  (push-mark (point-min) t t)
                  (goto-char (point-max)))
                (special-mode)
                (display-buffer
                 (current-buffer)
                 `((display-buffer-in-side-window)
                   (side . bottom)
                   (window-height . ,#'fit-window-to-buffer))))
            (message "gptel-request failed with message: %s"
                     (plist-get info :status)))))))

  )

(use-package gptel-quick
  :vc (gptel-quick :url "https://github.com/karthink/gptel-quick.git")
  :after embark
  :config
  (setq gptel-quick-backend gptel--google
        gptel-quick-model 'gemini-1.5-flash)
  :bind (:map embark-general-map ("?" . gptel-quick)))

(provide 'init-gpt)
