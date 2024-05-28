;; Azure OpenAI
(defvar azure-openai-api-host "beepboop.openai.azure.com")
(defvar azure-openai-api-path "/openai/deployments/%s/chat/completions?api-version=2024-02-01")
(defvar azure-openai-api-key (lambda () (auth-source-pass-get 'secret azure-openai-api-host)))

(use-package gptel
  :pin melpa
  :init
  (setq gptel--openai nil)

  :config
  (message "gptel is loaded")

  (defvar gptel--azure-gpt-4o
    (gptel-make-azure "Azure GPT-4o"
      :host azure-openai-api-host
      :key azure-openai-api-key
      :endpoint (format azure-openai-api-path "gpt-4o")
      :models '("gpt-4o")
      :stream t))

  (setq-default gptel-model "gpt-4o"
                gptel-backend gptel--azure-gpt-4o)

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
