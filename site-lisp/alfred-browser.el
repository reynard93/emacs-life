(require 'alfred-workflow)

(defvar alfred-workflow-browser "dev.yejun.browser"
  "The bundle id of Alfred Workflow - Browser.")

(defun alfred-browser-md-link ()
  (interactive)
  (alfred-workflow-run-trigger "md-link" alfred-workflow-browser))

(defun alfred-browser-org-link ()
  (interactive)
  (alfred-workflow-run-trigger "org-link" alfred-workflow-browser))

(defun alfred-browser-title ()
  (interactive)
  (alfred-workflow-run-trigger "title" alfred-workflow-browser))

(defun alfred-browser-url ()
  (interactive)
  (alfred-workflow-run-trigger "url" alfred-workflow-browser))

(provide 'alfred-browser)
