(defun alfred-workflow-run-trigger (trigger workflow &optional argument)
  "Run a specific TRIGGER in a given WORKFLOW with ARGUMENT in Alfred using AppleScript."
  (let* ((script (format "tell application id \"com.runningwithcrayons.Alfred\" to run trigger \"%s\" in workflow \"%s\"" trigger workflow))
         (script (if argument (format "%s with argument \"%s\"" script argument) script)))
    (do-applescript script)))

(provide 'alfred-workflow)
