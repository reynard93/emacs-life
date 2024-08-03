(defun alfred-workflow-run-trigger (trigger workflow)
  "Run a specific TRIGGER in a given WORKFLOW in Alfred using AppleScript."
  (let ((script (format "tell application id \"com.runningwithcrayons.Alfred\" to run trigger \"%s\" in workflow \"%s\"" trigger workflow)))
    (do-applescript script)))

(provide 'alfred-workflow)
