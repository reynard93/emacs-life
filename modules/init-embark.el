(use-package embark
  :demand t
  :init
  ;; Display commands under a prefix with C-h
  (setq prefix-help-command #'embark-prefix-help-command)
  (unbind-key "C-h C-h")

  :config
  (message "embark is loaded")

  (defun +embark/jira-target-link (project)
    "Target a JIRA link at point of the form PROJECT-1234."
    (save-excursion
      (when-let* ((bounds (bounds-of-thing-at-point 'symbol))
                  (start (car bounds))
                  (end (cdr bounds))
                  (str (buffer-substring-no-properties start end)))
        (when (string-match (format "%s-\\([[:digit:]]+\\)" project) str)
          `(url ,(format "https://fariaedu.atlassian.net/browse/%s-%s" project (match-string 1 str))
                ,start . ,end)))))

  (with-eval-after-load 'embark
    (add-to-list 'embark-target-finders (lambda () (+embark/jira-target-link "OA"))))

  :custom
  (embark-cycle-key "C-;")
  (embark-indicators
   '(embark-minimal-indicator
     embark-highlight-indicator
     embark-isearch-highlight-indicator))

  :bind (([remap describe-bindings] . embark-bindings)
         ("C-;" . embark-act)
         ("M-." . embark-dwim)
         :map vertico-map
         ("C-;" . embark-act)
         ("C-c C-;" . embark-export)
         ("C-c C-l" . embark-collect)))

(use-package embark-consult
  :after (embark consult)
  :config
  (message "embark-consult is loaded")
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-embark)
