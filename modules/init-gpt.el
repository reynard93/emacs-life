(use-package gptel
  :pin melpa
  :init
  (defvar gptel--openai
    (gptel-make-azure "OpenAI"
      :host "beepboop.openai.azure.com"
      :endpoint "/openai/deployments/gpt-4o/chat/completions?api-version=2024-02-01"
      :key (lambda () (auth-source-pass-get 'secret "beepboop.openai.azure.com"))
      :models '("gpt-4o")
      :stream t))

  (defvar gptel--kagi
    (gptel-make-kagi "Kagi"
      :key (lambda () (auth-source-pass-get 'secret "api.kagi.com"))))

  (defvar gptel--groq
    (gptel-make-openai "Groq"
      :host "api.groq.com"
      :endpoint "/openai/v1/chat/completions"
      :key (lambda () (auth-source-pass-get 'secret "api.groq.com"))
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

  :custom
  (gptel-max-tokens 700)
  (gptel-default-mode 'org-mode)
  :bind (("C-c C-<return>" . gptel-menu)
         ("C-c <return>" . +gptel/send)
         ("C-c C-g" . gptel-abort)
         :map gptel-mode-map
         ("C-c C-x t" . gptel-set-topic)
         ("M-n" . gptel-end-of-response)))

(provide 'init-gpt)
