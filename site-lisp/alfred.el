(defun alfred-workflow-run-trigger (trigger workflow &optional argument)
  "Run a specific TRIGGER in a given WORKFLOW with ARGUMENT in Alfred using AppleScript."
  (let* ((script (format "tell application id \"com.runningwithcrayons.Alfred\" to run trigger \"%s\" in workflow \"%s\"" trigger workflow))
         (script (if argument (format "%s with argument \"%s\"" script argument) script)))
    (do-applescript script)))

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

(provide 'alfred)
