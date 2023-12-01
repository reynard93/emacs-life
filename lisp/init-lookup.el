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
  (let ((query (read-string "Search for: " (thing-at-point 'symbol t))))
    (browse-url (format "https://kagi.com/search?q=%s" (url-encode-url query)))))

(defvar google-translate-target-lang nil)
(defvar google-translate-target-langs
  '(("Arabic" . "ar")
    ("German" . "de")
    ("English" . "en")
    ("Spanish" . "es")
    ("French" . "fr")
    ("Chinese Simplified" . "zh-CN")
    ("Chinese Traditional" . "zh-TW")))

(defun yejun/google-translate (source-lang target-lang)
  (let ((query (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (thing-at-point 'symbol t))))
    (browse-url (format "https://translate.google.com/?sl=%s&tl=%s&text=%s&op=translate"
                        source-lang target-lang (url-encode-url query)))))

(defun yejun/google-translate-guess-source-lang (&optional force-select)
  (interactive "P")
  (let* ((filename (buffer-file-name))
         (source-lang (if filename (file-name-base filename) "en"))
         (target-lang-full (if (or force-select (not google-translate-target-lang))
                               (completing-read "Select target language: " (mapcar 'car google-translate-target-langs))
                             (car (rassoc google-translate-target-lang google-translate-target-langs))))
         (target-lang (cdr (assoc target-lang-full google-translate-target-langs))))
    (setq google-translate-target-lang target-lang)
    (yejun/google-translate source-lang target-lang)))

(defun yejun/google-translate-guess-source-lang-force-select ()
  (interactive)
  (yejun/google-translate-guess-source-lang t))

(provide 'init-lookup)
