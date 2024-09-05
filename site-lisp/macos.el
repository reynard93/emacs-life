(defun macos-reveal-in-finder ()
  (interactive)
  (if-let ((filename (buffer-file-name)))
      (start-process "finder" nil "open" "-R" filename)
    (user-error "Buffer is not visiting any file")))

(defun macos-reveal-project-in-finder ()
  (interactive)
  (when-let* ((project-root-dir
               (condition-case nil
                   (project-root (project-current))
                 (error nil)))
              (filename (expand-file-name project-root-dir)))
    (start-process "finder" nil "open" "-R" filename)))

(defun macos-notify (title body &optional sound-name)
  (let* ((sound (or sound-name "Default"))
         (script (format "display notification \"%s\" with title \"%s\" sound name \"%s\""
                         body title sound)))
    (do-applescript script)))

(defun macos-dark-mode-p ()
  (let ((script "tell application \"System Events\"
                     tell appearance preferences
                         if (dark mode) then
                             return \"true\"
                         else
                             return \"false\"
                         end if
                     end tell
                 end tell"))
    (string-equal "true" (do-applescript script))))

(defun vscode-goto-file-at-point ()
  "Open the file at point in Visual Studio Code at the current line and column."
  (interactive)
  (when-let* ((filename (buffer-file-name))
              (line (line-number-at-pos))
              (column (current-column))
              (command (format "code --goto %s:%d:%d" filename line (+ column 1))))
    (start-process-shell-command "vscode" nil command)))

(provide 'macos)
