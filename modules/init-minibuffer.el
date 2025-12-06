(use-package vertico
  :init
  (vertico-mode 1)
  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)
  ;; Hide commands in M-x which do not work in the current mode.
  (setq read-extended-command-predicate #'command-completion-default-include-p)
  ;; Do not allow the cursor in the minibuffer prompt.
  (setq minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  :bind (:map vertico-map
         ("C-x ." . vertico-repeat)
         ("M-z" . vertico-suspend)
         ("RET" . vertico-directory-enter)
         ("DEL" . vertico-directory-delete-char)
         ("M-DEL" . vertico-directory-delete-word)
         ("M-q" . vertico-quick-insert)
         ("C-q" . vertico-quick-exit))

  :hook
  (minibuffer-setup . vertico-repeat-save)
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :custom
  (vertico-count 15)
  (vertico-cycle t)
  (vertico-multiform-categories
   '((embark-keybinding grid)
     (jinx grid (vertico-grid-annotate . 20))))
  :config
  (vertico-multiform-mode 1))

(use-package ffap
  :ensure nil
  :bind ("M-m" . ffap-menu)
  :config
  ;; Disable `ffap-menu's *Completions* buffer* because it's uncessary with vertico.
  (advice-add 'ffap-menu-ask :around
              (lambda (&rest args)
                (cl-letf (((symbol-function #'minibuffer-completion-help)
                           #'ignore))
                  (apply args)))))

(use-package marginalia
  :config
  (marginalia-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

(use-package consult
  :init
  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  :bind
  (([remap goto-line] . consult-goto-line)
   ([remap bookmark-jump] . consult-bookmark)
   ([remap switch-to-buffer] . consult-buffer)
   ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
   ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
   ([remap switch-to-buffer-other-tab] . consult-buffer-other-tab)
   ([remap project-switch-to-buffer] . consult-project-buffer)
   ("M-X" . consult-mode-command)
   ("M-y" . consult-yank-pop)
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store)
   :map goto-map
   ("i" . consult-imenu)
   ("I" . consult-imenu-multi)
   ("m" . consult-mark)
   ("k" . consult-global-mark)
   ("o" . consult-outline)
   ("r" . consult-register)
   ("f" . consult-flycheck)
   ("a" . consult-org-agenda)
   ("h" . consult-org-heading)
   :map search-map
   ("l" . consult-line)
   ("L" . consult-line-multi)
   ("r" . consult-ripgrep)
   ("e" . consult-isearch-history)
   :map minibuffer-local-map
   ("M-s" . consult-history)
   ("M-r" . consult-history)) ;; okay with it, but missing a lot of keybindings from centaur emacs

  :custom
  (consult-fd-args "fd --ignore-case --full-path --color=never"))

(use-package consult-dir
  :bind
  (("C-x C-d" . consult-dir)
   :map minibuffer-local-completion-map
   ("C-x C-d" . consult-dir)
   ("C-x C-j" . consult-dir-jump-file)))

;; Consult-Notes for easy access to notes

(use-package consult-notes
  :custom
  (consult-notes-denote-display-keywords-indicator "_")
  :bind
  (("C-c w g" . consult-notes)
   ("C-c w s" . consult-notes-search-in-all-notes))
  :init
  (consult-notes-denote-mode))



(use-package embark
  :demand t
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (unbind-key "C-h C-h")
  :bind
  (([remap describe-bindings] . embark-bindings)
   ("C-;" . embark-act)
   ("M-." . embark-dwim)
   :map minibuffer-local-map
   ("C-;" . embark-act)
   ("C-c C-;" . embark-export)
   ("C-c C-l" . embark-collect)
   ("M-." . my-embark-preview)
   :map embark-file-map
   ("t" . find-file-other-tab)
   ("T" . find-file-other-frame)
   :map embark-buffer-map
   ("t" . switch-to-buffer-other-tab)
   ("T" . switch-to-buffer-other-frame))
  :config
  ;; Manual preview for non-Consult commands using Embark
  (defun my-embark-preview ()
    "Previews candidate in vertico buffer, unless it's a consult command."
    (interactive)
    (unless (bound-and-true-p consult--preview-function)
      (save-selected-window
        (let ((embark-quit-after-action nil))
          (embark-dwim)))))
  (with-eval-after-load 'which-key
    (defun embark-which-key-indicator ()
      "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
      (lambda (&optional keymap targets prefix)
        (if (null keymap)
            (which-key--hide-popup-ignore-command)
          (which-key--show-keymap
           (if (eq (plist-get (car targets) :type) 'embark-become)
               "Become"
             (format "Act on %s '%s'%s"
                     (plist-get (car targets) :type)
                     (embark--truncate-target (plist-get (car targets) :target))
                     (if (cdr targets) "…" "")))
           (if prefix
               (pcase (lookup-key keymap prefix 'accept-default)
                 ((and (pred keymapp) km) km)
                 (_ (key-binding prefix 'accept-default)))
             keymap)
           nil nil t (lambda (binding)
                       (not (string-suffix-p "-argument" (cdr binding))))))))

    (setq embark-indicators
          '(embark-which-key-indicator
            embark-highlight-indicator
            embark-isearch-highlight-indicator))

    (defun embark-hide-which-key-indicator (fn &rest args)
      "Hide the which-key indicator immediately when using the completing-read prompter."
      (which-key--hide-popup-ignore-command)
      (let ((embark-indicators
             (remq #'embark-which-key-indicator embark-indicators)))
        (apply fn args)))

    (advice-add #'embark-completing-read-prompter
                :around #'embark-hide-which-key-indicator))
  :custom
  (embark-indicators
   '(embark-minimal-indicator
     embark-highlight-indicator
     embark-isearch-highlight-indicator)))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-minibuffer)
