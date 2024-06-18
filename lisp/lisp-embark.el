(defun +embark/jira-target-link (project)
  "Target a JIRA link at point of the form PROJECT-1234."
  (save-excursion
    (let* ((start (progn (skip-chars-backward "[:alnum:]-") (point)))
           (end (progn (skip-chars-forward "[:alnum:]-") (point)))
           (str (buffer-substring-no-properties start end)))
      (save-match-data
        (when (string-match (format "%s-\\([[:alnum:]]+\\)" project) str)
          `(url
            ,(format "https://fariaedu.atlassian.net/browse/%s"
                     (match-string 1 str))
            ,start . ,end))))))

(with-eval-after-load 'embark
  (add-to-list 'embark-target-finders (lambda () (+embark/jira-target-link "OA"))))

(provide 'lisp-embark)
