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
         :map goto-map
         ("e" . consult-compile-error)
         ("f" . consult-flymake)
         ("b" . consult-bookmark)
         ("l" . ffap-menu)
         ("m" . consult-mark)
         ("M" . consult-global-mark)
         :map search-map
         ("i" . consult-imenu)
         ("I" . consult-imenu-multi)
         ("l" . consult-line)
         ("L" . consult-line-multi)))

(use-package consult-dir
  :pin melpa
  :defer t
  :after consult
  :config
  (message "consult-dir is loaded"))

(provide 'init-minibuffer)
