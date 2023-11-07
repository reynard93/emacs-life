(savehist-mode 1)

(use-package vertico
  :init
  (message "vertico is loaded")
  (vertico-mode)
  :custom
  (vertico-cycle t)
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous))
  :hook (minibuffer-setup . vertico-repeat-save))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :config
  (message "vertico-directory is loaded")
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package marginalia
  :after vertico
  :config
  (message "marginalia is loaded")
  (marginalia-mode))

(use-package orderless
  :after vertico
  :config
  (message "orderless is loaded")
  :custom
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

(use-package embark
  :after vertico
  :config
  (message "embark is loaded")
  :bind (("C-;" . embark-act)
         :map vertico-map
         ("C-;" . embark-act)
         ("C-c C-;" . embark-export)
         ("C-c C-l" . embark-collect)))

(use-package embark-consult
  :config
  (message "embark-consult is loaded")
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package which-key
  :defer 1
  :config
  (message "which-key is loaded")
  (which-key-mode 1)
  :custom
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-min-display-lines 6)
  (which-key-min-display-columns nil)
  (which-key-add-column-padding 1))

(defun yejun/search-buffer ()
  (interactive)
  (consult-line))

(defalias 'yejun/search-buffer 'consult-line)
(defalias 'yejun/search-project 'consult-ripgrep)

(defun yejun/search-buffer-for-symbol-at-point ()
  (interactive)
  (consult-line (thing-at-point 'symbol)))

(defun yejun/search-project-for-symbol-at-point ()
  (interactive)
  (if-let* ((project (project-current))
            (root-dir (project-root project)))
      (consult-ripgrep root-dir (thing-at-point 'symbol))
    (message "You are not in a project.")))

(provide 'init-minibuffer)
