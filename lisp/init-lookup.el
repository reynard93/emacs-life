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

(provide 'init-lookup)
