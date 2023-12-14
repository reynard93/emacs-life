(use-package vertico
  :demand t
  :config
  (message "vertico is loaded")
  (vertico-mode 1)
  :custom
  (vertico-cycle t)
  :bind ( :map vertico-map
          ("C-j" . vertico-next)
          ("C-k" . vertico-previous))
  :hook (minibuffer-setup . vertico-repeat-save))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :config
  (message "vertico-directory is loaded")
  :bind ( :map vertico-map
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

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
  :custom
  (embark-cycle-key "C-;")
  :config
  (message "embark is loaded")

  ;; Open any buffer by splitting any window
  ;; https://karthinks.com/software/fifteen-ways-to-use-embark/#open-any-buffer-by-splitting-any-window
  (eval-when-compile
    (defmacro +embark/aw-action (fn)
      `(defun ,(intern (concat "+embark/aw-" (symbol-name fn))) ()
         ,(format "Open %s buffer selected with ace-window" (symbol-name fn))
         (interactive)
         (with-demoted-errors "%s"
           (require 'ace-window)
           (let ((aw-dispatch-always t))
             (aw-switch-to-window (aw-select nil))
             (call-interactively (symbol-function ',fn)))))))

  (define-key embark-file-map     (kbd "o") (+embark/aw-action find-file))
  (define-key embark-buffer-map   (kbd "o") (+embark/aw-action switch-to-buffer))
  (define-key embark-bookmark-map (kbd "o") (+embark/aw-action bookmark-jump))

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
  :defer 1
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
