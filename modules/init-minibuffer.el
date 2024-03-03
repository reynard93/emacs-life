(use-package vertico
  :demand t
  :preface
  (setq enable-recursive-minibuffers t) ; M-x in M-x

  ;; Ensure minibuffer prompt is read-only and cannot be modified
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  :config
  (message "vertico is loaded")
  (vertico-mode 1)

  ;; Disable `ffap-menu's completion buffer
  (advice-add 'ffap-menu-ask :around
              (lambda (&rest args)
                (cl-letf (((symbol-function #'minibuffer-completion-help)
                           #'ignore))
                  (apply args))))

  :custom
  (vertico-cycle t)
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind ( :map vertico-map
          ("C-j" . vertico-next)
          ("C-k" . vertico-previous)))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :config
  (message "vertico-directory is loaded")
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :bind ( :map vertico-map
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word)))

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
  :after vertico
  :config
  (message "consult is loaded")
  :bind (([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap bookmark-jump] . consult-bookmark)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         ([remap goto-line] . consult-goto-line)
         ([remap recentf-open-files] . consult-recent-file)
         ([remap imenu] . consult-imenu)
         ("M-y" . consult-yank-pop)
         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)
         ("M-g m" . consult-mark)
         ("M-g M" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings in `search-map'
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)))

(use-package consult-dir
  :pin melpa
  :defer t
  :after consult
  :config
  (message "consult-dir is loaded"))

(use-package embark
  :demand t
  :init
  (setq which-key-use-C-h-commands nil
        prefix-help-command #'embark-prefix-help-command)

  :config
  (message "embark is loaded")

  :custom
  (embark-cycle-key "C-;")

  :bind (([remap describe-bindings] . embark-bindings)
         ("C-;" . embark-act)
         :map vertico-map
         ("C-;" . embark-act)
         ("C-c C-;" . embark-export)
         ("C-c C-l" . embark-collect)))

(use-package embark-consult
  :config
  (message "embark-consult is loaded")
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package which-key
  :config
  (message "which-key is loaded")
  (which-key-mode 1)
  :custom
  (which-key-sort-order #'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-min-display-lines 6)
  (which-key-min-display-columns nil)
  (which-key-add-column-padding 1))

(provide 'init-minibuffer)
