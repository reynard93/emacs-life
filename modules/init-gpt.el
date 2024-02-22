;; Azure OpenAI
(defvar azure-openai-api-host "beepboop.openai.azure.com")
(defvar azure-openai-api-path "/openai/deployments/%s/chat/completions?api-version=2023-12-01-preview")
(defvar azure-openai-api-key (lambda () (auth-source-pass-get 'secret azure-openai-api-host)))

;; Google Gemini
(defvar gemini-api-host "generativelanguage.googleapis.com")
(defvar gemini-api-key (lambda () (auth-source-pass-get 'secret gemini-api-host)))

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

  (defvar gptel--kagi
    (gptel-make-kagi
     "Kagi"
     :key (lambda () (auth-source-pass-get 'secret "api.kagi.com"))
     :stream nil))

  (setq-default gptel-model "gpt-4"
                gptel-backend gptel--azure-gpt-4)

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
  (gptel-max-tokens 700)
  (gptel-default-mode 'org-mode)
  (gptel-prompt-prefix-alist '((org-mode . "* ")))
  (gptel-directives
   '((default . "You are a large language model living in Emacs and a helpful assistant. Respond concisely.")
     (ffmpeg . "You are an FFmpeg expert. Provide code and only code as output without any additional text, prompt or note.")
     (imagemagick . "You are an ImageMagick expert. Provide code and only code as output without any additional text, prompt or note.")))

  :hook (gptel-mode . visual-line-mode)
  :bind (("C-c C-<return>" . gptel-menu)
         ("C-c <return>" . +gptel/send)
         :map gptel-mode-map
         ("C-c C-x t" . gptel-set-topic)
         ("M-n" . gptel-end-of-response)))

(use-package chatgpt-shell
  :pin melpa
  :defer t
  :config
  (message "chatgpt-shell is loaded")

  :custom
  (chatgpt-shell-welcome-function nil)
  (chatgpt-shell-openai-key azure-openai-api-key)
  (chatgpt-shell-model-version (cl-position "gpt-3.5-turbo" chatgpt-shell-model-versions :test 'string=))

  ;; Azure OpenAI
  (chatgpt-shell-api-url-base (format "https://%s" azure-openai-api-host))
  (chatgpt-shell-api-url-path (format azure-openai-api-path "gpt-35-turbo"))
  (chatgpt-shell-auth-header (lambda () (format "api-key: %s" (chatgpt-shell-openai-key)))))

(use-package kagi
  :pin melpa
  :defer t
  :config
  (message "kagi is loaded")
  :custom
  (kagi-api-token (lambda () (auth-source-pass-get 'secret "api.kagi.com")))
  :bind ( :map embark-url-map
          ("K" . kagi-summarize-url)))

(provide 'init-gpt)
