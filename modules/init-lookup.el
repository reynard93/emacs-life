(use-package osx-dictionary
  :pin melpa
  :defer t
  :config
  (message "osx-dictionary is loaded")
  (evil-set-initial-state 'osx-dictionary-mode 'emacs))

(use-package dash-at-point
  :pin melpa
  :defer t
  :config
  (message "dash-at-point is loaded"))

(use-package deadgrep
  :defer t
  :pin melpa
  :config
  (message "deadgrep is loaded")
  (evil-set-initial-state 'deadgrep-mode 'emacs))

(defun +lookup/search-online ()
  (interactive)
  (let* ((default-query
          (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (thing-at-point 'word t)))
         (query (read-string "Search for: " default-query)))
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

(defun +lookup/google-translate (source-lang target-lang)
  (let ((query (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (thing-at-point 'symbol t))))
    (browse-url (format "https://translate.google.com/?sl=%s&tl=%s&text=%s&op=translate"
                        source-lang target-lang (url-encode-url query)))))

(defun +lookup/google-translate-guess-source-lang (&optional arg)
  (interactive "P")
  (let* ((filename (buffer-file-name))
         (source-lang (if filename (file-name-base filename) "en"))
         (target-lang-full (if (or arg (not google-translate-target-lang))
                               (completing-read "Select target language: " (mapcar 'car google-translate-target-langs))
                             (car (rassoc google-translate-target-lang google-translate-target-langs))))
         (target-lang (cdr (assoc target-lang-full google-translate-target-langs))))
    (setq google-translate-target-lang target-lang)
    (+lookup/google-translate source-lang target-lang)))

(defun +lookup/google-translate-guess-source-lang-force-select ()
  (interactive)
  (+lookup/google-translate-guess-source-lang t))

(provide 'init-lookup)
