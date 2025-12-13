;;; -*- lexical-binding: t -*-

;; good resource: https://wikemacs.org/wiki/Ruby
;; https://stackoverflow.com/questions/7989090/emacs-ruby-autocomplete-almost-working
;; the guy map7 rails config is godly?
(use-package ruby-ts-mode
  :ensure nil
  :defer t
  :init
  ;; NOTE: `ruby-indent-level' affects `ruby-mode'.  `ruby-ts-mode' uses its own
  ;; tree-sitter indentation variables.
  (setq ruby-indent-level 2
        ruby-indent-tabs-mode nil
        ruby-align-chained-calls t)
  (when (boundp 'ruby-ts-mode-indent-offset)
    (setq ruby-ts-mode-indent-offset 2))
  :hook (ruby-ts-mode . (lambda ()
                          (setq-local tab-width 2
                                      indent-tabs-mode nil)
                          ;; You disabled `electric-indent-mode' globally; enable it
                          ;; locally so RET indents in Ruby buffers.
                          (electric-indent-local-mode 1))))

(use-package bundler
  :defer t)

(use-package rake
  :defer t
  :custom
  (rake-completion-system 'default))

;; RSpec
(use-package rspec-mode
  :diminish
  :custom
  (rspec-command-options "--fail-fast --color")
  :hook (dired-mode . rspec-dired-mode))

;; Run a Ruby process in a buffer
(use-package inf-ruby
  :hook ((ruby-ts-mode . inf-ruby-minor-mode)
         (compilation-filter . inf-ruby-auto-enter)))

(defun enable-rspec-for-spec-files ()
  (when (and buffer-file-name
             (string-match-p "\\.spec\\.rb\\'" buffer-file-name))
    (rspec-mode)))

(add-hook 'ruby-mode-hook #'enable-rspec-for-spec-files)
(add-hook 'ruby-ts-mode-hook #'enable-rspec-for-spec-files)

(use-package rubocop
  :hook ruby-ts-mode)

;; https://matklad.github.io/2024/10/14/missing-ide-feature.html
;; uses tree-sitter, supported langs: rust, c++, js, python
(use-package auto-hide
  :ensure (:host github :repo "ultronozm/auto-hide.el" :depth nil)
  :config
  ;; Override the body extraction function specifically for Ruby also has to be called manually
  (defun my-auto-hide-ruby-methods ()
    "Hide all Ruby methods in the current buffer."
    (interactive)
    (when (and (eq major-mode 'ruby-ts-mode) (not hs-minor-mode))
      (hs-minor-mode 1))

    (when (eq major-mode 'ruby-ts-mode)
      (let* ((root-node (treesit-buffer-root-node))
             (query-string "(method) @method")
             (captures (treesit-query-capture root-node query-string)))

        (dolist (capture captures)
          (let* ((node (cdr capture))
                 (children (treesit-node-children node))
                 (body-node (seq-find (lambda (n)
                                        (equal (treesit-node-type n) "body_statement"))
                                      children)))

            (when body-node
              (let ((start (1- (treesit-node-start body-node)))
                    (end (treesit-node-end body-node)))
                (save-excursion
                  (goto-char start)
                  (hs-hide-block-at-point nil (list start end))))))))))

  ;; Replace auto-hide-hide-all for Ruby
  (advice-add 'auto-hide-hide-all :around
              (lambda (orig-fun)
                (if (eq major-mode 'ruby-ts-mode)
                    (my-auto-hide-ruby-methods)
                  (funcall orig-fun))))
  (global-auto-hide-mode))

(add-hook 'ruby-ts-mode-hook 'my-auto-hide-ruby-methods)

;; NOTE: robe removed as it's redundant with eglot+ruby-lsp which provides superior LSP features
(provide 'init-ruby)
