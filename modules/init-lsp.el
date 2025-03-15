;; keeping it simple with eglot
;; accidentally had this file toggled with init-eglot and server kept restarting itself and didnt work
;; https://www.ovistoica.com/blog/2024-7-05-modern-emacs-typescript-web-tsx-config
;; cargo install emacs-lsp-booster. Make sure to include the cargo bin folder in your $PATH.

(use-package lsp-mode
  :ensure t
  :commands (lsp-format-buffer lsp-organize-imports)
  :diminish "LSP"
  :preface
  (when (executable-find "emacs-lsp-booster")
    (defun lsp-booster--advice-json-parse (old-fn &rest args)
      "Try to parse bytecode instead of json."
      (or
       (when (equal (following-char) ?#)
         (let ((bytecode (read (current-buffer))))
           (when (byte-code-function-p bytecode)
             (funcall bytecode))))
       (apply old-fn args)))
    (advice-add (if (progn (require 'json)
                           (fboundp 'json-parse-buffer))
                    'json-parse-buffer
                  'json-read)
                :around
                #'lsp-booster--advice-json-parse)

    (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
      "Prepend emacs-lsp-booster command to lsp CMD."
      (let ((orig-result (funcall old-fn cmd test?)))
        (if (and (not test?)                             ;; for check lsp-server-present?
                 (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
                 lsp-use-plists
                 (not (functionp 'json-rpc-connection))  ;; native json-rpc
                 (executable-find "emacs-lsp-booster"))
            (progn
              (message "Using emacs-lsp-booster for %s!" orig-result)
              (cons "emacs-lsp-booster" orig-result))
          orig-result)))
    (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

  (defun os/lsp-gopls-hook ()
    (lsp-register-custom-settings '(("gopls.completeUnimported" t t)
                                    ("gopls.staticcheck" t t))))

  :hook ((lsp-mode . lsp-diagnostics-mode)
         (lsp-mode . lsp-enable-which-key-integration)
         ((tsx-ts-mode
           typescript-ts-mode
           json-ts-mode
           js-ts-mode
           java-ts-mode
           go-ts-mode
           python-ts-mode
           prisma-ts-mode
           clojure-mode
           clojurec-mode
           clojurescript-mode
           go-ts-mode) . lsp)
         (go-ts-mode . os/lsp-gopls-hook))

  :custom
  (lsp-completion-provider :none) ;; we use Corfu
  (lsp-diagnostics-provider :flycheck)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil) ; IMPORTANT! Use only for debugging! Drastically affects performance
  (lsp-keep-workspace-alive nil) ; Close LSP server if all project buffers are closed
  (lsp-idle-delay 0.5)   ; Debounce timer for `after-change-function'
  ;; core
  (lsp-enable-xref t)    ; Use xref to find references
  (lsp-auto-configure t) ; Used to decide between current active servers
  (lsp-eldoc-enable-hover t) ; Display signature information in the echo area
  (lsp-enable-dap-auto-configure t)     ; Debug support
  (lsp-enable-file-watchers nil) ; This basically means, if you change
                                        ; the branch, should LSP reload
                                        ; instantly - it's a big performance
                                        ; overhead
  (lsp-enable-folding nil)     ; I disable folding since I use origami
  (lsp-enable-imenu t)
  (lsp-progress-spinner-type 'progress-bar-filled)
  (lsp-enable-indentation nil)    ; I use prettier
  (lsp-enable-links nil)          ; No need since we have `browse-url'
  (lsp-enable-on-type-formatting nil)   ; Prettier handles this
  (lsp-enable-suggest-server-download t) ; Useful prompt to download LSP providers
  (lsp-enable-symbol-highlighting t) ; Shows usages of symbol at point in the current buffer
  (lsp-enable-text-document-color nil)  ; This is Treesitter's job

  (lsp-ui-sideline-show-hover nil) ; Sideline used only for diagnostics
  (lsp-ui-sideline-diagnostic-max-lines 20) ; 20 lines since typescript errors can be quite big
  ;; completion
  (lsp-completion-enable t)
  (lsp-completion-enable-additional-text-edit t) ; Ex: auto-insert an import for a completion candidate
  (lsp-enable-snippet t)    ; Important to provide full JSX completion
  (lsp-completion-show-kind t)         ; Optional
  ;; headerline
  (lsp-headerline-breadcrumb-enable t) ; Optional, I like the breadcrumbs
  (lsp-headerline-breadcrumb-enable-diagnostics nil) ; Don't make them red, too noisy
  (lsp-headerline-breadcrumb-enable-symbol-numbers nil)
  (lsp-headerline-breadcrumb-icons-enable nil)
  ;; modeline
  (lsp-modeline-code-actions-enable nil) ; Modeline should be relatively clean
  (lsp-modeline-diagnostics-enable nil) ; Already supported through `flycheck'
  (lsp-modeline-workspace-status-enable nil) ; Modeline displays "LSP" when lsp-mode is enabled
  (lsp-signature-doc-lines 1) ; Don't raise the echo area. It's distracting
  (lsp-ui-doc-use-childframe t)        ; Show docs for symbol at point
  (lsp-eldoc-render-all nil) ; This would be very useful if it would respect `lsp-signature-doc-lines', currently it's distracting
  ;; lens
  (lsp-lens-enable nil)                 ; Optional, I don't need it
  ;; semantic
  (lsp-semantic-tokens-enable nil) ; Related to highlighting, and we defer to treesitter

  :config
  ;; Disable `lsp-mode' in `git-timemachine-mode'
  (defun my-lsp--init-if-visible (fn &rest args)
    (unless (bound-and-true-p git-timemachine-mode)
      (apply fn args)))
  (advice-add #'lsp--init-if-visible :around #'my-lsp--init-if-visible)

  ;; Enable `lsp-mode' in sh/bash/zsh
  (defun my-lsp-bash-check-sh-shell (&rest _)
    (and (memq major-mode '(sh-mode bash-ts-mode))
         (memq sh-shell '(sh bash zsh))))
  (advice-add #'lsp-bash-check-sh-shell :override #'my-lsp-bash-check-sh-shell)
  (add-to-list 'lsp-language-id-configuration '(bash-ts-mode . "shellscript"))

  ;; Display icons
  (when (icons-displayable-p)
    (defun my-lsp-icons-get-symbol-kind (fn &rest args)
      (and (icons-displayable-p) (apply fn args)))
    (advice-add #'lsp-icons-get-by-symbol-kind :around #'my-lsp-icons-get-symbol-kind)

    ;; For `lsp-headerline'
    (defun my-lsp-icons-get-by-file-ext (fn &rest args)
      (and (icons-displayable-p) (apply fn args)))
    (advice-add #'lsp-icons-get-by-file-ext :around #'my-lsp-icons-get-by-file-ext)

    (defun my-lsp-icons-get-by-file-ext (file-ext &optional feature)
      (when (and file-ext
                 (lsp-icons--enabled-for-feature feature))
        (nerd-icons-icon-for-extension file-ext)))
    (advice-add #'lsp-icons-get-by-file-ext :override #'my-lsp-icons-get-by-file-ext)

    (defvar lsp-symbol-alist
      '((misc          nerd-icons-codicon "nf-cod-symbol_namespace" :face font-lock-warning-face)
        (document      nerd-icons-codicon "nf-cod-symbol_file" :face font-lock-string-face)
        (namespace     nerd-icons-codicon "nf-cod-symbol_namespace" :face font-lock-type-face)
        (string        nerd-icons-codicon "nf-cod-symbol_string" :face font-lock-doc-face)
        (boolean-data  nerd-icons-codicon "nf-cod-symbol_boolean" :face font-lock-builtin-face)
        (numeric       nerd-icons-codicon "nf-cod-symbol_numeric" :face font-lock-builtin-face)
        (method        nerd-icons-codicon "nf-cod-symbol_method" :face font-lock-function-name-face)
        (field         nerd-icons-codicon "nf-cod-symbol_field" :face font-lock-variable-name-face)
        (localvariable nerd-icons-codicon "nf-cod-symbol_variable" :face font-lock-variable-name-face)
        (class         nerd-icons-codicon "nf-cod-symbol_class" :face font-lock-type-face)
        (interface     nerd-icons-codicon "nf-cod-symbol_interface" :face font-lock-type-face)
        (property      nerd-icons-codicon "nf-cod-symbol_property" :face font-lock-variable-name-face)
        (indexer       nerd-icons-codicon "nf-cod-symbol_enum" :face font-lock-builtin-face)
        (enumerator    nerd-icons-codicon "nf-cod-symbol_enum" :face font-lock-builtin-face)
        (enumitem      nerd-icons-codicon "nf-cod-symbol_enum_member" :face font-lock-builtin-face)
        (constant      nerd-icons-codicon "nf-cod-symbol_constant" :face font-lock-constant-face)
        (structure     nerd-icons-codicon "nf-cod-symbol_structure" :face font-lock-variable-name-face)
        (event         nerd-icons-codicon "nf-cod-symbol_event" :face font-lock-warning-face)
        (operator      nerd-icons-codicon "nf-cod-symbol_operator" :face font-lock-comment-delimiter-face)
        (template      nerd-icons-codicon "nf-cod-symbol_snippet" :face font-lock-type-face)))

    (defun my-lsp-icons-get-by-symbol-kind (kind &optional feature)
      (when (and kind
                 (lsp-icons--enabled-for-feature feature))
        (let* ((icon (cdr (assoc (lsp-treemacs-symbol-kind->icon kind) lsp-symbol-alist)))
               (args (cdr icon)))
          (apply (car icon) args))))
    (advice-add #'lsp-icons-get-by-symbol-kind :override #'my-lsp-icons-get-by-symbol-kind)

    (setq lsp-headerline-arrow (nerd-icons-octicon "nf-oct-chevron_right"
                                                   :face 'lsp-headerline-breadcrumb-separator-face)))

  (setenv "PATH" (concat
                  "/usr/local/bin" path-separator
                  (getenv "PATH")))

  :init
  (setq lsp-use-plists t)

  :bind
  (:map lsp-mode-map
   ("C-c C-d" . lsp-describe-thing-at-point)
   ([remap lsp-treemacs-errors-list] . consult-lsp-diagnostics)
   ([remap consult-imenu] . consult-lsp-file-symbols)
   ([remap xref-find-apropos] . consult-lsp-symbols)
   ([remap xref-find-definitions] . lsp-find-definition)
   ([remap xref-find-apropos] . lsp-find-references)))

(use-package consult-lsp
  :after lsp-mode
  :bind (:map lsp-mode-map
         ("C-M-." . consult-lsp-symbols)))

(use-package lsp-completion
  :after lsp-mode
  :ensure nil
  :hook ((lsp-mode . lsp-completion-mode)))

(use-package lsp-ui
  :custom-face
  (lsp-ui-sideline-code-action ((t (:inherit warning))))
  :pretty-hydra
  ((:title (pretty-hydra-title "LSP UI" 'faicon "nf-fa-rocket" :face 'nerd-icons-green)
    :color amaranth :quit-key ("q" "C-g"))
   ("Doc"
    (("d e" (progn
              (lsp-ui-doc-enable (not lsp-ui-doc-mode))
              (setq lsp-ui-doc-enable (not lsp-ui-doc-enable)))
      "enable" :toggle lsp-ui-doc-mode)
     ("d s" (setq lsp-ui-doc-include-signature (not lsp-ui-doc-include-signature))
      "signature" :toggle lsp-ui-doc-include-signature)
     ("d t" (setq lsp-ui-doc-position 'top)
      "top" :toggle (eq lsp-ui-doc-position 'top))
     ("d b" (setq lsp-ui-doc-position 'bottom)
      "bottom" :toggle (eq lsp-ui-doc-position 'bottom))
     ("d p" (setq lsp-ui-doc-position 'at-point)
      "at point" :toggle (eq lsp-ui-doc-position 'at-point))
     ("d h" (setq lsp-ui-doc-header (not lsp-ui-doc-header))
      "header" :toggle lsp-ui-doc-header)
     ("d f" (setq lsp-ui-doc-alignment 'frame)
      "align frame" :toggle (eq lsp-ui-doc-alignment 'frame))
     ("d w" (setq lsp-ui-doc-alignment 'window)
      "align window" :toggle (eq lsp-ui-doc-alignment 'window)))
    "Sideline"
    (("s e" (progn
              (lsp-ui-sideline-enable (not lsp-ui-sideline-mode))
              (setq lsp-ui-sideline-enable (not lsp-ui-sideline-enable)))
      "enable" :toggle lsp-ui-sideline-mode)
     ("s h" (setq lsp-ui-sideline-show-hover (not lsp-ui-sideline-show-hover))
      "hover" :toggle lsp-ui-sideline-show-hover)
     ("s d" (setq lsp-ui-sideline-show-diagnostics (not lsp-ui-sideline-show-diagnostics))
      "diagnostics" :toggle lsp-ui-sideline-show-diagnostics)
     ("s s" (setq lsp-ui-sideline-show-symbol (not lsp-ui-sideline-show-symbol))
      "symbol" :toggle lsp-ui-sideline-show-symbol)
     ("s c" (setq lsp-ui-sideline-show-code-actions (not lsp-ui-sideline-show-code-actions))
      "code actions" :toggle lsp-ui-sideline-show-code-actions)
     ("s i" (setq lsp-ui-sideline-ignore-duplicate (not lsp-ui-sideline-ignore-duplicate))
      "ignore duplicate" :toggle lsp-ui-sideline-ignore-duplicate))
    "Action"
    (("h" backward-char "←")
     ("j" next-line "↓")
     ("k" previous-line "↑")
     ("l" forward-char "→")
     ("C-a" mwim-beginning-of-code-or-line nil)
     ("C-e" mwim-end-of-code-or-line nil)
     ("C-b" backward-char nil)
     ("C-n" next-line nil)
     ("C-p" previous-line nil)
     ("C-f" forward-char nil)
     ("M-b" backward-word nil)
     ("M-f" forward-word nil)
     ("c" lsp-ui-sideline-apply-code-actions "apply code actions"))))
  :bind (("C-c u" . lsp-ui-imenu)
         :map lsp-ui-mode-map
         ("M-<f6>" . lsp-ui-hydra/body)
         ("s-<return>" . lsp-ui-sideline-apply-code-actions)
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references))
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-delay 0.1
        lsp-ui-doc-show-with-cursor (not (display-graphic-p))
        lsp-ui-imenu-auto-refresh 'after-save
        lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                              ,(face-foreground 'font-lock-string-face)
                              ,(face-foreground 'font-lock-constant-face)
                              ,(face-foreground 'font-lock-variable-name-face)))
  ;; Set correct color to borders
  (defun my-lsp-ui-doc-set-border ()
    "Set the border color of lsp doc."
    (setq lsp-ui-doc-border
          (if (facep 'posframe-border)
              (face-background 'posframe-border nil t)
            (face-background 'region nil t))))
  (my-lsp-ui-doc-set-border)
  (add-hook 'after-load-theme-hook #'my-lsp-ui-doc-set-border t)
  :config
  (with-no-warnings
    ;; Display peek in child frame if possible
    ;; @see https://github.com/emacs-lsp/lsp-ui/issues/441
    (defvar lsp-ui-peek--buffer nil)
    (defun lsp-ui-peek--peek-display (fn src1 src2)
      (if (childframe-workable-p)
          (-let* ((win-width (frame-width))
                  (lsp-ui-peek-list-width (/ (frame-width) 2))
                  (string (-some--> (-zip-fill "" src1 src2)
                            (--map (lsp-ui-peek--adjust win-width it) it)
                            (-map-indexed 'lsp-ui-peek--make-line it)
                            (-concat it (lsp-ui-peek--make-footer)))))
            (setq lsp-ui-peek--buffer (get-buffer-create " *lsp-peek--buffer*"))
            (posframe-show lsp-ui-peek--buffer
                           :string (mapconcat 'identity string "")
                           :min-width (frame-width)
                           :internal-border-color (face-background 'posframe-border nil t)
                           :internal-border-width 1
                           :poshandler #'posframe-poshandler-frame-center))
        (funcall fn src1 src2)))
    (defun lsp-ui-peek--peek-destroy (fn)
      (if (childframe-workable-p)
          (progn
            (when (bufferp lsp-ui-peek--buffer)
              (posframe-hide lsp-ui-peek--buffer))
            (setq lsp-ui-peek--last-xref nil))
        (funcall fn)))
    (advice-add #'lsp-ui-peek--peek-new :around #'lsp-ui-peek--peek-display)
    (advice-add #'lsp-ui-peek--peek-hide :around #'lsp-ui-peek--peek-destroy)

    ;; Handle docs
    (defun my-lsp-ui-doc--handle-hr-lines nil
      (let (bolp next before after)
        (goto-char 1)
        (while (setq next (next-single-property-change (or next 1) 'markdown-hr))
          (when (get-text-property next 'markdown-hr)
            (goto-char next)
            (setq bolp (bolp)
                  before (char-before))
            (delete-region (point) (save-excursion (forward-visible-line 1) (point)))
            (setq after (char-after (1+ (point))))
            (insert
             (concat
              (and bolp (not (equal before ?\n)) (propertize "\n" 'face '(:height 0.5)))
              (propertize "\n" 'face '(:height 0.5))
              (propertize " "
                          ;; :align-to is added with lsp-ui-doc--fix-hr-props
                          'display '(space :height (1))
                          'lsp-ui-doc--replace-hr t
                          'face `(:background ,(face-foreground 'font-lock-comment-face nil t)))
              ;; :align-to is added here too
              (propertize " " 'display '(space :height (1)))
              (and (not (equal after ?\n)) (propertize " \n" 'face '(:height 0.5)))))))))
    (advice-add #'lsp-ui-doc--handle-hr-lines :override #'my-lsp-ui-doc--handle-hr-lines)))

(provide 'init-lsp)
