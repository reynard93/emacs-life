(use-package time
  :ensure nil
  :init
  (setq zoneinfo-style-world-list
        '(("America/Vancouver" "Vancouver")
          ("America/Chicago" "Portland")
          ("Europe/London" "London")
          ("Europe/Kyiv" "Kyiv")
          ("Asia/Shanghai" "Shanghai")))
  :custom
  (world-clock-time-format "%a %d %b %R %Z"))

(use-package browser-hist
  :load-path "vendor/browser-hist"
  :commands (browser-hist-search)
  :defer t
  :config
  (message "browser-hist is loaded")
  :custom
  (browser-hist-default-browser 'firefox))

(provide 'init-misc)
