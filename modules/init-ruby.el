(use-package ruby-ts-mode
  :ensure nil
  :defer t)

(use-package inf-ruby
  :hook (ruby-ts-mode . inf-ruby-minor-mode)
  :bind (:map inf-ruby-minor-mode-map ("C-c C-s" . inf-ruby-console-auto))
  :custom
  (inf-ruby-console-environment "development")
  :config
  ;; Reserve "C-c C-r" for `rubocop-mode-map'.
  (unbind-key "C-c C-r" inf-ruby-minor-mode-map))

(use-package bundler
  :defer t)

(use-package rake
  :defer t
  :custom
  (rake-completion-system 'default))

(use-package rspec-mode
  :hook ruby-ts-mode)

(use-package rubocop
  :hook ruby-ts-mode)

;; https://matklad.github.io/2024/10/14/missing-ide-feature.html
;; uses tree-sitter, supported langs: rust, c++, js, python
(use-package auto-hide
  :elpaca (:host github :repo "ultronozm/auto-hide")
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
(provide 'init-ruby)
