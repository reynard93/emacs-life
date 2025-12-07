;;; -*- lexical-binding: t -*-

;; good resource: https://wikemacs.org/wiki/Ruby
;; https://stackoverflow.com/questions/7989090/emacs-ruby-autocomplete-almost-working
;; the guy map7 rails config is godly?
(use-package ruby-ts-mode
  :ensure nil
  :defer t)

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

;; Ruby YARD comments
(use-package yard-mode
  :diminish
  :hook (ruby-ts-mode . yard-mode))

;; Ruby refactoring helpers
(use-package ruby-refactor
  :diminish
  :hook (ruby-ts-mode . ruby-refactor-mode-launch))

;; Yet Another RI interface for Emacs
(use-package yari
  :bind (:map ruby-ts-mode-map ([f1] . yari)))

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


;; enh-ruby-mode features that enhance the coding experience (sometimes indentatio doesn't do what you expect)
;; NOTE: enh-ruby-mode removed - using ruby-ts-mode which is built-in, faster, and has better LSP integration

;; either use rinari or projectile-rails (note rinari is unmaintained, use projectile-rails now)
;; using this fork bcz https://github.com/asok/projectile-rails/pull/169 it fixes handle of filenames with multiple .'s
                                        ;TODO am missing some config such as sql something and yasnippet for it idw use this anymore lol
;; (use-package projectile-rails
;;   :ensure
;;   :config
;;   ((projectile-rails-global-mode)
;; ))



;; Common tasks to do in the background
;; bpor is similar to aysnc-shell-cmd except:
;; bpr spawns processes asynchronously without displaying output buffers.
;; bpr shows progress messages for running processes in echo area.
;; bpr can display buffer with process output in case of errors.
;; bpr can use projectile for assigning process directory.
;; bpr can format process output (understands ansi escape codes).
;; it's possible to set different options for different processes.
;; bpr is very handy for running tests/builds, but you can run any processes with it.
(use-package bpr :ensure
  :config
  (setq bpr-colorize-output t)
  (setq bpr-close-after-success t)
  ;; Run tests on a rails project
  (defun rspec-tests ()
    "Spawns test process"
    (interactive)
    (let* ((bpr-scroll-direction -1) ;; scroll to the top of the output window (which is being shown in case of error)
           (bpr-close-after-success t)) ;; close error window after process ended successfully (if it's not already closed)
      (bpr-spawn "bundle exec rake spec")))
  )

; auto supply parens and 'end' as appropriate
;; https://github.com/ruby/elisp-ruby-electric/tree/master
;; not compatible with ruby-ts-mode

;; NOTE: robe removed as it's redundant with eglot+ruby-lsp which provides superior LSP features
(provide 'init-ruby)
