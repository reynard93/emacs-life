(use-package web-mode
  :defer t
  :mode "\.erb\'"
  :mode "\.vue\'"
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(use-package css-mode
  :elpaca nil
  :defer t
  :custom
  (css-indent-offset 2))

(use-package js-mode
  :elpaca nil
  :defer t
  :custom
  (js-indent-level 2))

(use-package haml-mode
  :defer t)

(use-package coffee-mode
  :defer t
  :custom
  (coffee-tab-width 2))

(use-package emmet-mode
  :hook (web-mode heex-ts-mode))

(provide 'init-web)
