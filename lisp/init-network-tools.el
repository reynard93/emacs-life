(use-package chatgpt-shell
  :pin melpa
  :config
  (message "chatgpt-shell is loaded")
  :custom
  (chatgpt-shell-welcome-function nil)
  (chatgpt-shell-model-version (cl-position "gpt-4" chatgpt-shell-model-versions :test 'string=))
  (chatgpt-shell-openai-key (lambda () (auth-source-pick-first-password :host "beepboop.openai.azure.com")))

  ;; Azure OpenAI
  (chatgpt-shell-api-url-base "https://beepboop.openai.azure.com")
  (chatgpt-shell-api-url-path "/openai/deployments/gpt-4/chat/completions?api-version=2023-07-01-preview")
  (chatgpt-shell-auth-header (lambda () (format "api-key: %s" (chatgpt-shell-openai-key))))
  (chatgpt-shell-streaming t)

  :bind (("C-c z z" . chatgpt-shell)
         ("C-c z b" . chatgpt-shell-prompt)
         ("C-c z c" . chatgpt-shell-prompt-compose)
         ("C-c z s" . chatgpt-shell-send-region)
         ("C-c z S" . chatgpt-shell-send-and-review-region)
         ("C-c z e" . chatgpt-shell-explain-code)
         ("C-c z r" . chatgpt-shell-refactor-code)))

(use-package mastodon
  :pin nongnu
  :defer t
  :config
  (message "mastodon is loaded")
  :custom
  (mastodon-instance-url "https://hachyderm.io")
  (mastodon-active-user "goofansu"))

(use-package 0x0
  :pin melpa
  :defer t
  :config
  (message "0x0 is loaded")
  :bind ( :map embark-region-map
          ("U" . 0x0-upload-text)
          :map embark-file-map
          ("U" . 0x0-upload-file)))

(provide 'init-network-tools)
