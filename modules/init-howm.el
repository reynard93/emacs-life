;; multi notes per file with C-c , C  -> new note in current buffer (maybe good for dailies?)
(use-package howm
  :ensure
  :config
  (setq howm-home-directory "~/Notes")
  (setq howm-directory "~/Notes")
  (setq howm-keyword-file (expand-file-name ".howm-keys" howm-home-directory))
  (setq howm-history-file (expand-file-name ".howm-history" howm-home-directory))
  (setq howm-file-name-format "%Y/%m/%Y-%m-%d-%H%M%S.org") ; change the back .org to use orgmode default

  ;; Use ripgrep as grep
  (setq howm-view-use-grep t)
  (setq howm-view-grep-command "rg")
  (setq howm-view-grep-option "-nH --no-heading --color never")
  (setq howm-view-grep-extended-option nil)
  (setq howm-view-grep-fixed-option "-F")
  (setq howm-view-grep-expr-option nil)
  (setq howm-view-grep-file-stdin-option nil)

  ;; counsel-rg for howm
  (defun howm-list--counsel-rg (match)
    (if (string= match "")
        (howm-list-all)
      (if (or (null ivy--old-cands)
	          (equal ivy--old-cands '("No matches found")))
          (message "No match")
        (let ((howm-view-use-grep
	           #'(lambda (str file-list &optional fixed-p force-case-fold)
                   (mapcar
                  (lambda (cand)
		            (if (string-match "\\`\\(.*\\):\\([0-9]+\\):\\(.*\\)\\'" cand)
                        (let ((file (match-string-no-properties 1 cand))
			                  (line (match-string-no-properties 2 cand))
			                  (match-line (match-string-no-properties 3 cand)))
                          (list (expand-file-name file howm-directory)
                                (string-to-number line)
                                match-line))))
                  ivy--old-cands))))
          (howm-search ivy--old-re t)
          (riffle-set-place
           (1+ (cl-position match ivy--old-cands :test 'string=)))))))

  (defun howm-counsel-rg ()
    "Interactively grep for a string in your howm notes using rg."
    (interactive)
    (let ((default-directory howm-directory)
          (counsel-ag-base-command counsel-rg-base-command)
          (counsel-ag-command (counsel--format-ag-command "--glob=!*~" "%s")))
      (ivy-read "Search all (rg): "
	            #'counsel-ag-function
	            :dynamic-collection t
	            :keymap counsel-ag-map
	            :action #'howm-list--counsel-rg
	            :require-match t
	            :caller 'counsel-rg)))

  (define-key global-map (concat howm-prefix "r") 'howm-counsel-rg)

  ;; Default recent to sorting by mtime
  (advice-add 'howm-list-recent :after #'howm-view-sort-by-mtime)
  ;; Default all to sorting by creation, newest first
  (advice-add 'howm-list-all :after #'(lambda () (howm-view-sort-by-date t)))

  ;; howm names are unwieldy, use these to rename note buffers according to their title, makes switchign ezier
  (add-hook 'howm-mode-hook 'howm-mode-set-buffer-name)
  (add-hook 'after-save-hook 'howm-mode-set-buffer-name)

  ;; fix anti feature
  (define-key howm-menu-mode-map "\C-h" nil)
  (define-key riffle-summary-mode-map "\C-h" nil)
  (define-key howm-view-contents-mode-map "\C-h" nil)


  ;; Backspace is ok, but don't bind C-h.
  (define-key howm-menu-mode-map "\C-h" nil)
  (define-key riffle-summary-mode-map "\C-h" nil)
  (define-key howm-view-contents-mode-map "\C-h" nil)

  ;; Custom URLs
  ;; zotero://
  (add-to-list 'action-lock-default-rules
               (list "\\<zotero://\\S +" (lambda (&optional dummy)
                                         (browse-url (match-string-no-properties 0)))))
  ;; @bibtex
  (add-to-list 'action-lock-default-rules
               (list "\\s-\\(@\\([a-zA-Z0-9:-]+\\)\\)\\>"
                   (lambda (&optional dummy)
                     (browse-url (concat "zotero://select/items/bbt:"
                                         (match-string-no-properties 2))))
                   1))
  ;; make wiki-links jump to single title hit if possible
  (add-to-list 'action-lock-default-rules
               (list howm-wiki-regexp
                   (lambda (&optional dummy)
                     (let ((s (match-string-no-properties howm-wiki-regexp-pos)))
                       ;; letting create-p be nil here, howm-keyword-search-subr
                       ;; should check create-p after open-unique-p
                       (howm-keyword-search (concat "= " s) nil t)))
                   howm-wiki-regexp-hilit-pos))

  ;; Right-click in howmS to jump to file
  (defun howm-view-summary-at-mouse (e)
    (interactive "e")
    (mouse-set-point e)
    (riffle-summary-check t)
    (howm-view-summary-open t))
  (define-key howm-view-summary-mode-map [mouse-3] #'howm-view-summary-at-mouse))
