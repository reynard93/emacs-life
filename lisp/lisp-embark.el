;; Open any buffer by splitting any window
;; https://karthinks.com/software/fifteen-ways-to-use-embark/#open-any-buffer-by-splitting-any-window
(eval-when-compile
  (defmacro +embark--aw-action (fn)
    `(defun ,(intern (concat "+embark/aw-" (symbol-name fn))) ()
       ,(format "Open %s buffer selected with ace-window." (symbol-name fn))
       (interactive)
       (with-demoted-errors "%s"
         (require 'ace-window)
         (let ((aw-dispatch-always t))
           (aw-switch-to-window (aw-select nil))
           (call-interactively (symbol-function ',fn)))))))

(with-eval-after-load 'embark
  (keymap-set embark-file-map     "o" (+embark--aw-action find-file))
  (keymap-set embark-buffer-map   "o" (+embark--aw-action switch-to-buffer))
  (keymap-set embark-bookmark-map "o" (+embark--aw-action bookmark-jump)))

(defun +embark/jira-link ()
  "Target a link at point of the form jira:OA-1234."
  (save-excursion
    (let* ((start (progn (skip-chars-backward "[:alnum:]-:") (point)))
           (end (progn (skip-chars-forward "[:alnum:]-:") (point)))
           (str (buffer-substring-no-properties start end)))
      (save-match-data
        (when (string-match "jira:\\([[:alnum:]-]+\\)" str)
          `(url
            ,(format "https://fariaedu.atlassian.net/browse/%s"
                     (match-string 1 str))
            ,start . ,end))))))

(with-eval-after-load 'embark
  (add-to-list 'embark-target-finders #'+embark/jira-link))

(provide 'lisp-embark)
