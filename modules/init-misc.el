(use-package time
  :ensure nil
  :init
  (setq zoneinfo-style-world-list
        '(("America/Vancouver" "Vancouver")
          ("America/Chicago" "Portland")
          ("UTC" "UTC")
          ("Europe/London" "London")
          ("Europe/Kyiv" "Kyiv")
          ("Asia/Shanghai" "Shanghai")))
  :custom
  (world-clock-time-format "%F %T %z"))

(provide 'init-misc)
