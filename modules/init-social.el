;; tedious to install, use this https://tdlib.github.io/td/build.html?language=C
(use-package telega
  :ensure (telega
           :host github
           :repo "zevlg/telega.el"
           :branch "master"
           :files (:defaults "contrib" "etc" "server" "Makefile"))
  :defer t)

(provide 'init-social)
