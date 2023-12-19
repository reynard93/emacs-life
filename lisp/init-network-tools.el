(setq gpt-api-host "beepboop.openai.azure.com")
(setq gpt-api-path "/openai/deployments/%s/chat/completions?api-version=2023-07-01-preview")
(setq gpt-api-key (lambda () (auth-source-pick-first-password :host gpt-api-host)))

(use-package chatgpt-shell
  :pin melpa
  :defer t
  :config
  (message "chatgpt-shell is loaded")
  :custom
  (chatgpt-shell-welcome-function nil)
  (chatgpt-shell-openai-key gpt-api-key)
  (chatgpt-shell-model-version (cl-position "gpt-4" chatgpt-shell-model-versions :test 'string=))

  ;; Azure OpenAI
  (chatgpt-shell-api-url-base (format "https://%s" gpt-api-host))
  (chatgpt-shell-api-url-path (format gpt-api-path "gpt-4"))
  (chatgpt-shell-auth-header (lambda () (format "api-key: %s" (chatgpt-shell-openai-key))))
  (chatgpt-shell-streaming t))

(use-package gptel
  :pin melpa
  :defer t
  :config
  (message "gptel is loaded")
  (setq-default gptel-backend
                (gptel-make-azure
                 "Azure GPT-3.5"
                 :host gpt-api-host
                 :endpoint (format gpt-api-path "gpt-35-turbo")
                 :models '("gpt-3.5-turbo")
                 :stream t))
  :custom
  (gptel-api-key gpt-api-key)
  (gptel-max-tokens 400))

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
