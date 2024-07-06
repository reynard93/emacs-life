(use-package vertico
  :demand t
  :preface
  (setq enable-recursive-minibuffers t) ; M-x in M-x

  ;; Ensure minibuffer prompt is read-only and cannot be modified
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Disable `ffap-menu's completion buffer
  (advice-add 'ffap-menu-ask :around
              (lambda (&rest args)
                (cl-letf (((symbol-function #'minibuffer-completion-help)
                           #'ignore))
                  (apply args))))

  :config
  (message "vertico is loaded")
  (vertico-mode 1)

  :custom
  (vertico-cycle t)
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind ("C-x ." . vertico-repeat))

(use-package vertico-suspend
  :ensure nil
  :after vertico
  :config
  (message "vertico-suspend is loaded")
  :bind ("M-z" . vertico-suspend))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :config
  (message "vertico-directory is loaded")
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :bind ( :map vertico-map
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word)))

(use-package vertico-quick
  :ensure nil
  :after vertico
  :config
  (message "vertico-quick is loaded")
  :bind ( :map vertico-map
          ("M-q" . vertico-quick-insert)
          ("C-q" . vertico-quick-exit)))

(use-package vertico-multiform
  :ensure nil
  :after vertico
  :config
  (message "vertico-multiform is loaded")
  (vertico-multiform-mode 1)
  :custom
  (vertico-multiform-categories
   '((embark-keybinding grid))))

(use-package marginalia
  :after vertico
  :config
  (message "marginalia is loaded")
  (marginalia-mode 1))

(use-package orderless
  :after vertico
  :config
  (message "orderless is loaded")
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t
        completion-ignore-case t)
  :custom
  (orderless-smart-case nil)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :demand t
  :config
  (message "consult is loaded")
  :bind (("M-X" . consult-mode-command)
         ("M-y" . consult-yank-pop)
         :map ctl-x-map
         ("M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("b"   . consult-buffer)              ;; orig. switch-to-buffer
         ("4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("t b" . consult-buffer-other-tab)    ;; orig. switch-to-buffer-other-tab
         ("r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         :map goto-map
         ("e" . consult-compile-error)
         ("f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("g" . consult-goto-line)             ;; orig. goto-line
         ("M-g" . consult-goto-line)           ;; orig. goto-line
         ("o" . consult-outline)               ;; Alternative: consult-org-heading
         ("m" . consult-mark)
         ("k" . consult-global-mark)
         ("i" . consult-imenu)
         ("I" . consult-imenu-multi)
         :map search-map
         ("d" . consult-fd)
         ("r" . consult-ripgrep)
         ("l" . consult-line)
         ("L" . consult-line-multi)
         ("u" . consult-focus-lines)
         ("e" . consult-isearch-history)
         ("m" . consult-kmacro)
         ("a" . consult-org-agenda)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history)                 ;; orig. previous-matching-history-element
         :map org-mode-map
         ("M-g o" . consult-org-heading)))

(use-package consult-dir
  :pin melpa
  :defer t
  :after consult
  :config
  (message "consult-dir is loaded"))

(provide 'init-minibuffer)
