(use-package gptel
  :pin melpa
  :init
  (defvar gptel--openai nil
    "Override the variable to hide ChatGPT models")

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

  :config
  (setq gptel-model 'anthropic/claude-3.5-sonnet
        gptel-backend (gptel-make-openai "OpenRouter"
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

  (defun +gptel/send-all-buffers (text)
    "Send TEXT to all buffers where gptel-mode is active and execute `gpt-send'."
    (interactive "sEnter text: ")
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (bound-and-true-p gptel-mode)
          (save-excursion
            (goto-char (point-max))
            (insert text)
            (gptel-send)))))))

(use-package gptel-quick
  :vc (gptel-quick :url "https://github.com/karthink/gptel-quick.git")
  :after embark
  :config
  (setq gptel-quick-backend gptel--google
        gptel-quick-model 'gemini-1.5-flash)
  :bind (:map embark-general-map ("?" . gptel-quick)))

(provide 'init-gpt)
