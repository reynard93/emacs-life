(defcustom embark-jira-host nil
  "Your JIRA host domain.")

(defun embark-jira-target-link (project)
  "Target a JIRA link at point of the form PROJECT-1234."
  (save-excursion
    (when-let* ((bounds (bounds-of-thing-at-point 'symbol))
                (start (car bounds))
                (end (cdr bounds))
                (str (buffer-substring-no-properties start end)))
      (when (string-match (format "%s-\\([[:digit:]]+\\)" project) str)
        `(url ,(format "%s/browse/%s-%s" embark-jira-host project (match-string 1 str))
              ,start . ,end)))))

(provide 'embark-jira)
