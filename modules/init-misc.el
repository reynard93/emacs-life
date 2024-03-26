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

(provide 'init-misc)
