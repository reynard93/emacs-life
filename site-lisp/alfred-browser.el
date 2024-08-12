(require 'alfred-workflow)

(defvar alfred-workflow-browser "dev.yejun.browser"
  "The bundle id of Alfred Workflow - Browser.")

(defun alfred-browser-link-in-org-format ()
  (interactive)
  (alfred-workflow-run-trigger "org-link" alfred-workflow-browser))

(defun alfred-browser-link-in-markdown-format ()
  (interactive)
  (alfred-workflow-run-trigger "markdown-link" alfred-workflow-browser))

(defun alfred-browser-link-url ()
  (interactive)
  (alfred-workflow-run-trigger "link-url" alfred-workflow-browser))

(defun alfred-browser-link-title ()
  (interactive)
  (alfred-workflow-run-trigger "link-title" alfred-workflow-browser))

(defvar-keymap alfred-browser-prefix-map
  "m" #'alfred-browser-link-in-markdown-format
  "o" #'alfred-browser-link-in-org-format
  "t" #'alfred-browser-link-title
  "u" #'alfred-browser-link-url)

(provide 'alfred-browser)
