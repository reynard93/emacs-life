(use-package gptel
  :pin melpa
  :init
  (defvar gptel--openai nil
    "Override the variable to hide ChatGPT models")

  (defvar gptel--google
    (gptel-make-gemini "Google"
      :key (lambda () (auth-source-pass-get 'secret "api-key/gemini"))
      :stream t
      :models '(gemini-1.5-flash
                gemini-1.5-flash-8b
                gemini-1.5-pro)))

  (defvar gptel--openrouter
    (gptel-make-openai "OpenRouter"
      :host "openrouter.ai"
      :endpoint "/api/v1/chat/completions"
      :stream t
      :key (lambda () (auth-source-pass-get 'secret "api-key/openrouter"))
      :models '(anthropic/claude-3.5-sonnet
                anthropic/claude-3-5-haiku)))

  :bind
  (("C-c <return>" . gptel-send)
   ("C-c C-<return>" . gptel-menu)
   ("C-c M-<return>" . +gptel/send-all-buffers)
   :map embark-region-map
   ("g t" . +gptel/translate))

  :custom
  (gptel-default-mode 'org-mode)

  :config
  (setq gptel-model 'anthropic/claude-3-5-haiku
        gptel-backend gptel--openrouter)

  (defun +gptel/send-all-buffers (text)
    "Send TEXT to all buffers where gptel-mode is active and execute `gpt-send'."
    (interactive "sText: ")
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (bound-and-true-p gptel-mode)
          (save-excursion
            (goto-char (point-max))
            (insert text)
            (gptel-send))))))

  (defun +gptel/translate (text)
    "Translate TEXT into English using LLM.
If region is active, use it as TEXT; otherwise prompt for input.
Display the result in a side window with the content selected."
    (interactive "sText: ")
    (let ((gptel-backend gptel--google)
          (gptel-model 'gemini-1.5-flash))
      (gptel-request text
        :system "You're a language translator. Translate text into English, keep the original format and meaning."
        :callback
        (lambda (response info)
          (if response
              (with-current-buffer (get-buffer-create "*gptel-translate*")
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert response))
                (special-mode)
                (display-buffer
                 (current-buffer)
                 `((display-buffer-in-side-window)
                   (side . bottom)
                   (window-height . ,#'fit-window-to-buffer))))
            (message "gptel-request failed with message: %s"
                     (plist-get info :status))))))))

(use-package gptel-quick
  :vc (gptel-quick :url "https://github.com/karthink/gptel-quick.git")
  :bind (:map embark-general-map ("?" . gptel-quick))
  :config
  (setq gptel-quick-backend gptel--google
        gptel-quick-model 'gemini-1.5-flash-8b))

(provide 'init-gpt)
