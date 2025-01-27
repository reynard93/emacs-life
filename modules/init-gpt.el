(use-package gptel
  :pin melpa
  :init
  (defvar gptel--openai nil
    "Override the variable to hide ChatGPT models")

  (defvar gptel--google
    (gptel-make-gemini "Google"
      :key (lambda () (auth-source-pass-get 'secret "api-key/gemini"))
      :stream t))

  (defvar gptel--openrouter
    (gptel-make-openai "OpenRouter"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :stream t
      :key (lambda () (auth-source-pass-get 'secret "api-key/openrouter"))
      :models '(anthropic/claude-3.5-sonnet
                openai/gpt-4o-mini)))

  (defvar gptel--deepseek
    (gptel-make-openai "DeepSeek"
      :host "api.deepseek.com"
      :endpoint "/chat/completions"
      :stream t
      :key (lambda () (auth-source-pass-get 'secret "api-key/deepseek"))
      :models '(deepseek-chat)))

  (defvar gptel--kagi
    (gptel-make-kagi "Kagi"
      :key (lambda () (auth-source-pass-get 'secret "api-key/kagi"))))

  :bind
  (("C-c <return>" . gptel-send)
   ("C-c C-<return>" . gptel-menu)
   ("C-c M-<return>" . my/gptel-send-all-buffers)
   :map embark-region-map
   ("T" . my/gptel-translate))

  :custom
  (gptel-default-mode 'org-mode)

  :config
  (setq gptel-backend gptel--deepseek
        gptel-model 'deepseek-chat)

  (defun my/gptel-send-all-buffers (prompt)
    "Send PROMPT in all buffers where gptel-mode is active."
    (interactive "sPrompt: ")
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (bound-and-true-p gptel-mode)
          (save-excursion
            (goto-char (point-max))
            (insert prompt)
            (gptel-send))))))

  (defun my/gptel--callback-display-bottom (response info)
    (if response
        (with-current-buffer (get-buffer-create "*gptel-translate*")
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
               (plist-get info :status))))

  (defun my/gptel-translate (text)
    "Translate TEXT into English using LLM.
If region is active, use it as TEXT; otherwise prompt for input.
Display the result in a side window with the content selected."
    (interactive "sText: ")
    (let ((gptel-backend gptel--deepseek)
          (gptel-model 'deepseek-chat))
      (gptel-request text
        :system "You're an en-zh language translator. Keep the original format and meaning."
        :callback #'my/gptel--callback-display-bottom)))

  (defun my/gptel-summarize (text)
    "Summarize TEXT using Kagi."
    (interactive "sText: ")
    (let ((gptel-backend gptel--kagi)
          (gptel-model 'summerize:agnes))
      (gptel-request text
        :callback #'my/gptel--callback-display-bottom))))

(use-package gptel-quick
  :vc (gptel-quick :url "https://github.com/karthink/gptel-quick.git")
  :bind (:map embark-general-map ("?" . gptel-quick))
  :config
  (setq gptel-quick-backend gptel--openrouter
        gptel-quick-model 'openai/gpt-4o-mini))

(provide 'init-gpt)
