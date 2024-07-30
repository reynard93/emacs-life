(use-package vertico
  :init
  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt.
  (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  :custom
  (vertico-cycle t)
  :config
  (vertico-mode 1))

(use-package ffap
  :bind ("M-m" . ffap-menu)
  :config
  ;; Disable `ffap-menu's *Completions* buffer* because it's uncessary with vertico.
  (advice-add 'ffap-menu-ask :around
              (lambda (&rest args)
                (cl-letf (((symbol-function #'minibuffer-completion-help)
                           #'ignore))
                  (apply args)))))

(use-package vertico-repeat
  :ensure nil
  :after vertico
  :bind ("C-x ." . vertico-repeat)
  :hook (minibuffer-setup . vertico-repeat-save))

(use-package vertico-suspend
  :ensure nil
  :after vertico
  :bind ("M-z" . vertico-suspend))

(use-package vertico-directory
  :ensure nil
  :after vertico
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :bind ( :map vertico-map
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word)))

(use-package vertico-quick
  :ensure nil
  :after vertico
  :bind ( :map vertico-map
          ("M-q" . vertico-quick-insert)
          ("C-q" . vertico-quick-exit)))

(use-package vertico-multiform
  :ensure nil
  :after vertico
  :config
  (vertico-multiform-mode 1)
  :custom
  (vertico-multiform-categories
   '((embark-keybinding grid)
     (jinx grid (vertico-grid-annotate . 20)))))

(use-package marginalia
  :init
  (marginalia-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package consult
  :demand t
  :bind (([remap goto-line] . consult-goto-line)
         ([remap bookmark-jump] . consult-bookmark)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap switch-to-buffer-other-tab] . consult-buffer-other-tab)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         ("M-X" . consult-mode-command)
         ("M-y" . consult-yank-pop)
         :map goto-map
         ("i" . consult-imenu)
         ("I" . consult-imenu-multi)
         :map search-map
         ("r" . consult-ripgrep)
         ("l" . consult-line)
         ("L" . consult-line-multi)))

(use-package consult-dir
  :pin melpa
  :after (consult vertico)
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package embark
  :after vertico
  :demand t
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (unbind-key "C-h C-h")
  :custom
  (embark-indicators
   '(embark-minimal-indicator
     embark-highlight-indicator
     embark-isearch-highlight-indicator))
  :bind (([remap describe-bindings] . embark-bindings)
         ("C-;" . embark-act)
         ("M-." . embark-dwim)
         :map vertico-map
         ("C-;" . embark-act)
         ("C-c C-;" . embark-export)
         ("C-c C-l" . embark-collect)))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-minibuffer)
