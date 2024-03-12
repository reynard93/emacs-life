(defun +exercism/download (track exercise)
  (interactive
   (list (completing-read "Track Name: " '(elixir ruby))
         (read-string "Exercise Name: ")))
  (shell-command (format "exercism download --track=%s --exercise=%s" track exercise)))

(defun +exercism/submit ()
  (interactive)
  (let ((output-buffer "*exercism-output*"))
    (shell-command "exercism submit" output-buffer)
    (with-current-buffer output-buffer
      (goto-char (point-max))
      (forward-line -1)
      (kill-new (thing-at-point 'line)))
    (kill-buffer output-buffer)))

(provide 'lisp-exercism)
