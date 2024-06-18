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

(provide 'lisp-embark)
