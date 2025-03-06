;; You should only adopt Elisp code you need and understand to avoid Emacs Bankruptcy.
;; https://matklad.github.io/2024/10/14/missing-ide-feature.html
;; uses tree-sitter, supported langs: rust, c++, js, python

(use-package auto-hide
  :vc (auto-hide :url "https://github.com/ultronozm/auto-hide.el")
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

;; https://github.com/novoid/dot-emacs/blob/master/config.org (THIS config, refer to winow management)
;; the new spliting way the utility when I split the screen with C-x 2 or C-x 3, it opens the previous buffer instead of giving me two panes with the same buffer:
(defun my-vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun my-hsplit-last-buffer ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(bind-key "C-x 2" 'my-vsplit-last-buffer)
(bind-key "C-x 3" 'my-hsplit-last-buffer)
;; end new split way

(defun split-and-follow-horizontally ()
  "Split window horizontally and move to the new window."
  (interactive)
  (select-window (split-window-below)))

(defun split-and-follow-vertically ()
  "Split window vertically and move to the new window."
  (interactive)
  (select-window (split-window-right)))

(defun split-and-follow-horizontally-and-open (file)
  "Split window horizontally, move to the new window, and open FILE."
  (split-and-follow-horizontally)
  (find-file file))

(defun split-and-follow-vertically-and-open (file)
  "Split window vertically, move to the new window, and open FILE."
  (split-and-follow-vertically)
  (find-file file))

(with-eval-after-load 'embark
  (define-key embark-file-map (kbd "X") #'split-and-follow-horizontally-and-open)
  (define-key embark-file-map (kbd "V") #'split-and-follow-vertically-and-open))
;; end Embark new splitting ways

;; Add undo-tree

(use-package dape
  :ensure t
  :preface
  (setq dape-key-prefix "\C-c\C-a")
  :hook
  ;; Save breakpoints on quit
  (kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  (after-init . dape-breakpoint-load)
  ;; Pulse source line (performance hit)
  (dape-display-source . pulse-momentary-highlight-one-line)
  :custom
  (dape-buffer-window-arrangement 'right)
  (dape-adapter-dir (expand-file-name "debug-adapters" user-emacs-directory))
  (dape-inlay-hints t)
  :config
  (defun jjh/dape-buffer-matches-suffix (suffix)
    "Only include the buffer in the dape command if the buffer matches SUFFIX."
    (when (string-suffix-p suffix (dape-buffer-default))
      (dape-buffer-default))))

;; Dape config for RSpec over docker
;; before i struggled mightily with the files not being detected and unable to hook into it
(use-package dape
  :config
  (add-to-list 'dape-configs
               `(rdbg-attach-rails
                 modes (ruby-ts-mode)
                 prefix-remote rspec-docker-cwd
                 ;; Use the project root that the local buffer is
                 ;; attached to
                 prefix-local (lambda () (project-root (project-current)))
                 port 5678
                 :request "attach"
                 :localfs t)) ;; on yr project run 'bundle exec rdbg --port 5678 -O -n -c -- bin/rails server -p 3000'

  (add-to-list 'dape-configs
               `(rdbg-attach-rspec
                 modes (ruby-ts-mode)
                 prefix-remote rspec-docker-cwd
                 ;; Use the project root that the local buffer is
                 ;; attached to
                 prefix-local (lambda () (project-root (project-current))) ;; add /spec (i.e. entry of your spec)
                 port 5679
                 :request "attach"
                 :localfs t))) ;; on yr project run 'bundle exec rdbg --port 5678 -O -n -c -- rspec <path-to-spec>'
;; i.e. Run adapter: rdbg-attach-rspec prefix-local "/app/spec" for the bob project
;; example: bundle exec rdbg --port -n -c -- rspec ./spec/models/submission_spec.rb

(provide 'init-reynard)
