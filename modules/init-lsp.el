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
           ruby-ts-mode
           clojurescript-mode
           go-ts-mode) . lsp)
         (go-ts-mode . os/lsp-gopls-hook))

  :custom
  (lsp-completion-provider :none) ;; we use Corfu
  (lsp-diagnostics-provider :flycheck)
  (lsp-session-file (locate-user-emacs-file ".lsp-session"))
  (lsp-log-io nil) ; IMPORTANT! Use only for debugging! Drastically affects performance
  (lsp-keep-workspace-alive nil) ; Close LSP server if all project buffers are closed
  (lsp-idle-delay 0.2)   ; Reduce debounce timer for faster response
  (lsp-response-timeout 2) ; Shorter timeout for faster perceived performance
  ;; core
  (lsp-enable-xref t)    ; Use xref to find references
  (lsp-auto-configure t) ; Used to decide between current active servers
  (lsp-eldoc-enable-hover t) ; Display signature information in the echo area
  (lsp-enable-dap-auto-configure t)     ; Debug support
  (lsp-enable-file-watchers nil) ; This basically means, if you change
                                        ; the branch, should LSP reload
                                        ; instantly - it's a big performance
                                        ; overhead
  (lsp-file-watch-threshold 1000) ; Only watch up to 1000 files for changes
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
  ;; (lsp-ui-doc-use-childframe t)        ; Show docs for symbol at point
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

;; Optimize garbage collection during LSP operations
(defun my/lsp-gc-optimize ()
  "Optimize garbage collection settings for LSP mode."
  (setq-local gc-cons-threshold (* 1024 1024 100))) ; 100MB

(add-hook 'lsp-mode-hook #'my/lsp-gc-optimize)

(provide 'init-lsp)
