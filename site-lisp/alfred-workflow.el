(defvar alfred-workflow-browser "dev.yejun.browser"
  "The bundle id of Alfred Workflow - Browser.")

(defun alfred-workflow--run-trigger (trigger workflow)
  "Run a specific TRIGGER in a given WORKFLOW in Alfred using AppleScript."
  (let ((script (format "tell application id \"com.runningwithcrayons.Alfred\" to run trigger \"%s\" in workflow \"%s\"" trigger workflow)))
    (do-applescript script)))

(defun alfred-workflow-browser-link (&optional arg)
  "Returns link in org-mode format, if ARG is present, returns link
in markdown format."
  (interactive "P")
  (if arg
      (alfred-workflow--run-trigger "markdown-link" alfred-workflow-browser)
    (alfred-workflow--run-trigger "org-link" alfred-workflow-browser)))

(defun alfred-workflow-browser-link-url ()
  "Returns the URL of link."
  (interactive)
  (alfred-workflow--run-trigger "link-url" alfred-workflow-browser))

(defun alfred-workflow-browser-link-title ()
  "Returns the title of link."
  (interactive)
  (alfred-workflow--run-trigger "link-title" alfred-workflow-browser))

(defvar-keymap alfred-workflow-keymap
  "l" #'alfred-workflow-browser-link
  "t" #'alfred-workflow-browser-link-title
  "u" #'alfred-workflow-browser-link-url)

(provide 'alfred-workflow)
