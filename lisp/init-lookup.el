(use-package osx-dictionary
  :pin melpa
  :defer t
  :config
  (message "osx-dictionary is loaded")
  (evil-collection-init 'osx-dictionary))

(use-package dash-at-point
  :pin melpa
  :defer t
  :config
  (message "dash-at-point is loaded"))

(defun yejun/lookup-online ()
  (interactive)
  (let* ((url "https://kagi.com/search?q=%s")
         (query (read-string "Search for: " (thing-at-point 'symbol t))))
    (browse-url (format url query))))

(defun yejun/google-translate (source-lang target-lang)
  (let ((query (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (thing-at-point 'symbol t))))
    (browse-url (format "https://translate.google.com/?sl=%s&tl=%s&text=%s&op=translate"
                        source-lang target-lang (url-encode-url query)))))

(defvar google-translate-target-langs '("zh-CN" "zh-TW"))
(defun yejun/google-translate-guess-source-lang ()
  (interactive)
  (let* ((filename (buffer-file-name))
         (source-lang (if filename
                          (file-name-base filename)
                        "en"))
         (target-lang (completing-read "Select target language: " google-translate-target-langs)))
    (yejun/google-translate source-lang target-lang)))

(provide 'init-lookup)
