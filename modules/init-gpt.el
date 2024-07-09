(use-package gptel
  :pin melpa
  :init
  (defvar gptel--openai
    (gptel-make-azure "OpenAI"
      :host "beepboop.openai.azure.com"
      :endpoint "/openai/deployments/gpt-4o/chat/completions?api-version=2024-02-01"
      :key (lambda () (auth-source-pass-get 'secret "openai.azure.com/api-key/beepboop"))
      :models '("gpt-4o")
      :stream t))

  (defvar gptel--kagi
    (gptel-make-kagi "Kagi"
      :key (lambda () (auth-source-pass-get 'secret "kagi.com/api-key"))))

  (defvar gptel--groq
    (gptel-make-openai "Groq"
      :host "api.groq.com"
      :endpoint "/openai/v1/chat/completions"
      :key (lambda () (auth-source-pass-get 'secret "groq.com/api-key"))
      :models '("llama3-70b-8192")
      :stream t))
  
  (setq-default gptel-backend gptel--openai
                gptel-model "gpt-4o")

  :config
  (message "gptel is loaded")

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

  (cl-defmethod gptel--request-data ((_backend gptel-kagi) prompts)
    "Override `gptel-kagi's `gptel--request-data'. Set summary_type to takeaway
when using summarize models."
    (pcase-exhaustive gptel-model
      ("fastgpt"
       `(,@prompts :web_search t :cache t))
      ((and model (guard (string-prefix-p "summarize" model)))
       `(,@prompts :engine ,(substring model 10) :summary_type "takeaway"))))

  (defun +gptel/kagi-summarize-url (url)
    "Summarize URL using Kagi's Universal Summarizer."
    (interactive "sEnter URL: ")
    (let ((gptel-backend gptel--kagi)
          (gptel-model "summarize:agnes"))
      (gptel-request url
        :callback
        (lambda (response info)
          (if response
              (let ((output-name (format "*Kagi Summary: %s*" (plist-get (plist-get info :data) :url))))
                (with-current-buffer (get-buffer-create output-name)
                  (let ((inhibit-read-only t))
                    (erase-buffer)
                    (visual-line-mode 1)
                    (insert response)
                    (display-buffer (current-buffer))
                    (special-mode))))
            (message "gptel-request failed with message: %s"
                     (plist-get info :status)))))))

  :custom
  (gptel-max-tokens 700)
  (gptel-default-mode 'org-mode)
  :bind (("C-c C-<return>" . gptel-menu)
         ("C-c <return>" . +gptel/send)
         :map gptel-mode-map
         ("C-c C-x t" . gptel-set-topic)
         ("M-n" . gptel-end-of-response)
         :map embark-url-map
         ("=" . +gptel/kagi-summarize-url)))

(use-package gptel-quick
  :vc (gptel-quick :url "https://github.com/karthink/gptel-quick.git")
  :config
  (message "gptel-quick is loaded")
  :bind ( :map embark-general-map
          ("?" . gptel-quick)))

(provide 'init-gpt)
