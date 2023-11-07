(defun yejun/gh-pr-create ()
  (interactive)
  (shell-command "gh pr create -w"))

(defun yejun/gh-pr-view ()
  (interactive)
  (shell-command "gh pr view -w"))

(defun yejun/gist-region-or-buffer (&optional p)
  (interactive "P")
  (let ((filename (buffer-name))
        (output-buffer " *gist-output*")
        (public (if p " --public" "")))
    (shell-command-on-region
     (if (use-region-p) (region-beginning) (point-min))
     (if (use-region-p) (region-end) (point-max))
     (concat "gh gist create --filename " filename public " -")
     output-buffer)
    (with-current-buffer output-buffer
      (goto-char (point-max))
      (forward-line -1)
      (kill-new (thing-at-point 'line)))
    (kill-buffer output-buffer)))

(global-set-key (kbd "C-c b g") #'yejun/gist-region-or-buffer)

(provide 'init-github)
