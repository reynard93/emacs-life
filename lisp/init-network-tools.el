;; Azure OpenAI
(setq azure-openai-api-host "beepboop.openai.azure.com")
(setq azure-openai-api-path "/openai/deployments/%s/chat/completions?api-version=2023-07-01-preview")
(setq azure-openai-api-key (lambda () (auth-source-pick-first-password :host azure-openai-api-host)))

;; Google Gemini
(setq gemini-api-host "generativelanguage.googleapis.com")
(setq gemini-api-key (lambda () (auth-source-pick-first-password :host gemini-api-host)))

(use-package chatgpt-shell
  :pin melpa
  :config
  (message "chatgpt-shell is loaded")

  :custom
  (chatgpt-shell-welcome-function nil)
  (chatgpt-shell-openai-key azure-openai-api-key)
  (chatgpt-shell-model-version (cl-position "gpt-4" chatgpt-shell-model-versions :test 'string=))

  ;; Azure OpenAI
  (chatgpt-shell-api-url-base (format "https://%s" azure-openai-api-host))
  (chatgpt-shell-api-url-path (format azure-openai-api-path "gpt-4"))
  (chatgpt-shell-auth-header (lambda () (format "api-key: %s" (chatgpt-shell-openai-key))))

  :bind ( :map embark-region-map
          ("z e" . chatgpt-shell-explain-code)
          ("z r" . chatgpt-shell-refactor-code)
          ("z s" . chatgpt-shell-send-region)
          ("z S" . chatgpt-shell-send-and-review-region)))

(use-package gptel
  :pin melpa
  :config
  (message "gptel is loaded")
  (setq-default gptel-backend
                (gptel-make-azure
                 "Azure GPT-3.5"
                 :host azure-openai-api-host
                 :key azure-openai-api-key
                 :endpoint (format azure-openai-api-path "gpt-35-turbo")
                 :models '("gpt-3.5-turbo")
                 :stream t))

  (gptel-make-gemini
   "Gemini"
   :host gemini-api-host
   :key gemini-api-key
   :stream t)

  :bind ("s-g" . gptel-menu))

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
