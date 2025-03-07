(defun macos-reveal-in-finder ()
  (interactive)
  (if-let ((path (or (buffer-file-name)
                     (and (derived-mode-p 'dired-mode)
                          (expand-file-name default-directory)))))
      (start-process "finder" nil "open" "-R" path)
    (user-error "Buffer is not visiting a file or directory")))

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

;; remove goto file via ide vscode / rubymine
(provide 'macos)
