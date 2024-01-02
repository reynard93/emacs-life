;; Azure OpenAI
(defvar azure-openai-api-host "beepboop.openai.azure.com")
(defvar azure-openai-api-path "/openai/deployments/%s/chat/completions?api-version=2023-07-01-preview")
(defvar azure-openai-api-key (lambda () (auth-source-pass-get 'secret azure-openai-api-host)))

;; Google Gemini
(defvar gemini-api-host "generativelanguage.googleapis.com")
(defvar gemini-api-key (lambda () (auth-source-pass-get 'secret gemini-api-host)))

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

  :bind ( :prefix-map chatgpt-shell-prefix-map
          :prefix "C-c z"
          ("z" . chatgpt-shell)
          ("b" . chatgpt-shell-prompt)
          ("c" . chatgpt-shell-prompt-compose)
          ("e" . chatgpt-shell-explain-code)
          ("r" . chatgpt-shell-refactor-code)
          ("s" . chatgpt-shell-send-region)
          ("S" . chatgpt-shell-send-and-review-region)))

(use-package gptel
  :pin melpa
  :config
  (message "gptel is loaded")

  (defvar gptel--azure-gpt-35
    (gptel-make-azure
     "Azure GPT-3.5"
     :host azure-openai-api-host
     :key azure-openai-api-key
     :endpoint (format azure-openai-api-path "gpt-35-turbo")
     :models '("gpt-3.5-turbo")
     :stream t))

  (defvar gptel--azure-gpt-4
    (gptel-make-azure
     "Azure GPT-4"
     :host azure-openai-api-host
     :key azure-openai-api-key
     :endpoint (format azure-openai-api-path "gpt-4")
     :models '("gpt-4")
     :stream t))

  (defvar gptel--gemini
    (gptel-make-gemini
     "Gemini"
     :host gemini-api-host
     :key gemini-api-key
     :stream t))

  (require 'gptel-kagi)
  (defvar gptel--kagi
    (gptel-make-kagi
     "Kagi"
     :key (lambda () (auth-source-pass-get 'secret "api.kagi.com"))
     :stream nil))

  (setq-default gptel-model "gpt-3.5-turbo"
                gptel-backend gptel--azure-gpt-35)

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
     ((y-or-n-p "[gptel] Prompt has more than 2000 chars, really send?") (gptel-send arg))
     (t (message "[gptel] Request cancelled"))))

  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "Prompt: ")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "Response: ")

  :custom
  (gptel-max-tokens 400)
  (gptel-default-mode 'org-mode)
  (gptel-prompt-prefix-alist '((org-mode . "* ")))

  :bind (("C-c C-<return>" . gptel-menu)
         ("C-c <return>" . +gptel/send)
         :map gptel-mode-map
         ("C-c C-x t" . gptel-set-topic)
         ("M-n" . gptel-end-of-response))

  :hook (gptel-mode . visual-line-mode))

(use-package kagi
  :load-path "lisp/kagi"
  :commands (kagi-fastgpt-shell)
  :defer t
  :config
  (message "kagi is loaded")
  :custom
  (kagi-api-token (lambda () (auth-source-pass-get 'secret "api.kagi.com")))
  :bind ( :map embark-url-map
          ("K" . kagi-summarize-url)))

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
