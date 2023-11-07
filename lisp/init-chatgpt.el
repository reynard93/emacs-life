(use-package chatgpt-shell
  :pin melpa
  :defer t
  :config
  (message "chatgpt-shell is loaded")
  :custom
  (chatgpt-shell-welcome-function nil)
  (chatgpt-shell-model-version (cl-position "gpt-4" chatgpt-shell-model-versions :test 'string=))
  (chatgpt-shell-openai-key (lambda () (auth-source-pick-first-password :host "api.openai.com"))))

(push '("*chatgpt*"
        (display-buffer-in-direction)
        (direction . below)
        (window-height . 0.5))
      display-buffer-alist)

(provide 'init-chatgpt)
