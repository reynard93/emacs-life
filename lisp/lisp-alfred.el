(defvar alfred-workflow-browser "dev.yejun.browser"
  "The bundle id of Alfred Workflow - Browser.")

(defun +alfred/run-trigger-in-workflow (trigger workflow)
  "Run a specific TRIGGER in a given WORKFLOW in Alfred using AppleScript."
  (let ((script (format "tell application id \"com.runningwithcrayons.Alfred\" to run trigger \"%s\" in workflow \"%s\"" trigger workflow)))
    (do-applescript script)))

(defun +alfred/browser-link (&optional arg)
  "Returns link in org-mode format, if ARG is present, returns link
in markdown format."
  (interactive "P")
  (if arg
      (+alfred/run-trigger-in-workflow "markdown-link" alfred-workflow-browser)
    (+alfred/run-trigger-in-workflow "org-link" alfred-workflow-browser)))

(defun +alfred/browser-link-url ()
  "Returns the URL of link."
  (interactive)
  (+alfred/run-trigger-in-workflow "link-url" alfred-workflow-browser))

(defun +alfred/browser-link-title ()
  "Returns the title of link."
  (interactive)
  (+alfred/run-trigger-in-workflow "link-title" alfred-workflow-browser))

(provide 'lisp-alfred)
