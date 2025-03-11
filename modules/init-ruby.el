;; good resource: https://wikemacs.org/wiki/Ruby
;; https://stackoverflow.com/questions/7989090/emacs-ruby-autocomplete-almost-working
;; the guy map7 rails config is godly?
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

(use-package rvm.el
  :defer t
  :ensure (:host github :repo "senny/rvm.el" :files ("*.el")))

(use-package rspec-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.spec\\.rb\\'" . ruby-ts-mode))

  (defun enable-rspec-for-spec-files ()
    (when (and buffer-file-name
               (string-match-p "\\.spec\\.rb\\'" buffer-file-name))
      (rspec-mode)))

  (add-hook 'ruby-mode-hook #'enable-rspec-for-spec-files)
  (add-hook 'ruby-ts-mode-hook #'enable-rspec-for-spec-files)

  :custom
  (rspec-use-rvm t) ;; this requires rvm.el
  (rspec-command-options "--fail-fast --color"))

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

;; chruby - activates a project's ruby versio nwithin Emacs
;; make sure to install chruby with brew first
;;  load auto.sh in ~/.bashrc or ~/.zshrc: (https://github.com/postmodern/chruby) for auto switch when u cd btwn projects (shell) i think chruby does it auto for u in ruby
;; article that recommends using chruby instead of rbm or rbenv
;; https://stevemarshall.com/journal/why-i-use-chruby/
;; if using chruby also pair it with ruby-install
;; dont care for now , also asdf etc


;; enh-ruby-mode features that enhance the coding experience (sometimes indentatio doesn't do what you expect)

;; either use rinari or projectile-rails (note rinari is unmaintained, use projectile-rails now)
;; using this fork bcz https://github.com/asok/projectile-rails/pull/169 it fixes handle of filenames with multiple .'s
                                        ;TODO am missing some config such as sql something and yasnippet for it idw use this anymore lol
;; (use-package projectile-rails
;;   :ensure
;;   :config
;;   ((projectile-rails-global-mode)
;; ))

;; caveats to robe-mode
;; 1. only one project @/time with inf-ruby

(provide 'init-ruby)
