(use-package 0x0
  :pin melpa
  :defer t
  :config
  (message "0x0 is uploaded")
  :bind ( :map embark-region-map
          ("U" . 0x0-upload-text)
          :map embark-file-map
          ("U" . 0x0-upload-file)))

(provide 'init-upload)
