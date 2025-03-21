;; You should only adopt Elisp code you need and understand to avoid Emacs Bankruptcy.
;; It WILL be even better if certain packages that depends on system binaries are conditionally activated and installed based on 'executable-find'

;; https://github.com/purcell/color-theme-sanityinc-tomorrow
;; https://github.com/daut/miasma-theme.el


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

(use-package dape
  :ensure
  :bind (("<f5>" . dape))
  :custom (dape-buffer-window-arrangment 'right)
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
                 :localfs t)) ;; on yr project run 'bundle exec rdbg --port 5678 -O -n -c -- rspec <path-to-spec>'
  ;; i.e. Run adapter: rdbg-attach-rspec prefix-local "/app/spec" for the bob project
  ;; example: bundle exec rdbg --port -n -c -- rspec ./spec/models/submission_spec.rb
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
      (dape-buffer-default)))
  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-on-start-hooks
            (defun dape--save-on-start ()
              (save-some-buffers t t))))

;; terminates buffer automatically after inactivity of 30mins
(use-package buffer-terminator
  :ensure (:host github :url "https://github.com/jamescherti/buffer-terminator.el")
  :custom
  (buffer-terminator-verbose nil)
  :config
  (buffer-terminator-mode 1))

;; use this for now, maybe pair with https://codeberg.org/martianh/sentex
(setq sentence-end-double-space nil)

(use-package stillness-mode
  :config
  (stillness-mode +1))

(use-package vundo
  :commands (vundo)
  :bind
  (("C-x u" . vundo))
  :custom
  (vundo-roll-back-on-quit nil)
  (vundo--window-max-height 10)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package pinentry
  :config
  (pinentry-start))

;; requires installation of cmake
(use-package vterm :ensure
  :init
  (setq vterm-shell "/opt/homebrew/bin/fish"))
(use-package multi-vterm :ensure)

(setq project-vc-extra-root-markers '("package.json" "Gemfile" "global.d.ts"))

(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :config
  (use-package restclient-test
    :diminish
    :hook (restclient-mode . restclient-test-mode)))

;; Setup the amazing Prodigy
;; Add the lien below else requests will be very slow when Emacs naps
;; $ defaults write org.gnu.Emacs NSAppSleepDisabled -bool YES


(use-package org-download :ensure
  :hook((org-mode . org-download-enable)))

(use-package kubed
  :bind-keymap ("C-c k" . kubed-prefix-map))

(defun nuke-all-buffers ()
  "Kill all buffers, leaving *scratch* only."
  (interactive)
  (mapcar (lambda (x) (kill-buffer x)) (buffer-list))
  (delete-other-windows))

;; i love transparency but i cannot get alpha-background to work on the emacs i use
(use-package emacs
  :ensure nil
  :config
  (set-frame-parameter nil 'alpha '(92 . 85))
  (add-to-list 'default-frame-alist '(alpha . (92 . 85))))

(provide 'init-reynard)
