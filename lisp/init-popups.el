(push '("*scratch*"
        (display-buffer-in-direction)
        (direction . below)
        (window-height . 0.3))
      display-buffer-alist)

(push '("*Org Select*"
        (display-buffer-in-direction)
        (direction . below)
        (window-height . 0.3))
      display-buffer-alist)

(push '("*chatgpt*"
        (display-buffer-in-direction)
        (direction . below)
        (window-height . 0.5))
      display-buffer-alist)

(push '("*rspec-compilation*"
        (display-buffer-in-direction)
        (direction . below)
        (window-height . 0.5))
      display-buffer-alist)

(provide 'init-popups)
