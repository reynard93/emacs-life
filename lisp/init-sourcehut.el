(defun yejun/paste-region-or-buffer (&optional p)                                              
  (interactive "P")                                                                            
  (let ((filename (read-string "Enter filename: " (buffer-name)))                              
        (output-buffer " *paste-output*")                                                      
        (public (if p " --visibility public" "")))                                             
    (shell-command-on-region                                                                   
     (if (use-region-p) (region-beginning) (point-min))                                        
     (if (use-region-p) (region-end) (point-max))                                              
     (concat "hut paste create --name \"" filename "\"" public)                                
     output-buffer)                                                                            
    (with-current-buffer output-buffer                                                         
      (goto-char (point-max))                                                                  
      (forward-line -1)                                                                        
      (kill-new (thing-at-point 'line)))                                                       
    (kill-buffer output-buffer)))                                                              

(global-set-key (kbd "C-c b p") #'yejun/paste-region-or-buffer)

(provide 'init-sourcehut)
