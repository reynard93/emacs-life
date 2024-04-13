(defvar-local +org-preview-toggle nil
  "Non-nil if +org-preview-toggle is enabled.")

(defun +org/preview-toggle ()
  (interactive)
  (if +org-preview-toggle
      (progn
        (logos-focus-mode -1)
        (org-indent-mode -1)
        (org-superstar-mode -1)
        (setq-local org-hide-emphasis-markers nil)
        (setq-local +org-preview-toggle nil))
    (progn
      (logos-focus-mode 1)
      (org-indent-mode 1)
      (org-superstar-mode 1)
      (setq-local org-hide-emphasis-markers t)
      (setq-local +org-preview-toggle t))))

(defun +org/browse-files ()
  (interactive)
  (+project/browse-files org-directory))

;; https://github.com/doomemacs/doomemacs/blob/4d072ce888577b023774460f6036abefcd0a1fa6/modules/lang/org/autoload/org-refile.el
(defun +org/refile-to-current-file (arg &optional file)
  "Refile current heading to elsewhere in the current buffer.
If prefix ARG, copy instead of move."
  (interactive "P")
  (let ((org-refile-targets `((,file :maxlevel . 10)))
        (org-refile-use-outline-path t)
        (org-refile-keep arg)
        current-prefix-arg)
    (call-interactively #'org-refile)))

(defun +org/refile-to-file (arg file)
  "Refile current heading to a particular org file.
If prefix ARG, copy instead of move."
  (interactive
   (list current-prefix-arg
         (read-file-name "Select file to refile to: "
                         default-directory
                         (buffer-file-name (buffer-base-buffer))
                         t nil
                         (lambda (f) (string-match-p "\\.org$" f)))))
  (+org/refile-to-current-file arg file))

;; https://github.com/doomemacs/doomemacs/blob/986398504d09e585c7d1a8d73a6394024fe6f164/modules/lang/org/autoload/org.el#L318-L336
(defun +org/toggle-last-clock (arg)
  "Toggles last clocked item.

Clock out if an active clock is running (or cancel it if prefix ARG is non-nil).

If no clock is active, then clock into the last item. See `org-clock-in-last' to
see how ARG affects this command."
  (interactive "P")
  (require 'org-clock)
  (cond ((org-clocking-p)
         (if arg
             (org-clock-cancel)
           (org-clock-out)))
        ((and (null org-clock-history)
              (or (org-on-heading-p)
                  (org-at-item-p))
              (y-or-n-p "No active clock. Clock in on current item?"))
         (org-clock-in))
        ((org-clock-in-last arg))))

(provide 'lisp-org)
