(use-package web-mode
  :pin nongnu
  :defer t
  :config
  (message "web-mode is loaded")
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(use-package css-mode
  :ensure nil
  :defer t
  :config
  (message "css-mode is loaded")
  :custom
  (css-indent-offset 2))

(use-package js-mode
  :ensure nil
  :defer t
  :config
  (message "js-mode is loaded")
  :custom
  (js-indent-level 2))

(use-package haml-mode
  :pin melpa
  :defer t
  :config
  (message "haml-mode is loaded"))

(use-package coffee-mode
  :pin melpa
  :defer t
  :config
  (message "coffee-mode is loaded"))

(provide 'init-web)
