;; You should only adopt Elisp code you need and understand to avoid Emacs Bankruptcy.

;; Set both alpha parameters, affects text also, cannot get alpha-background o work
(set-frame-parameter nil 'alpha '(90 . 85))
;; https://github.com/purcell/color-theme-sanityinc-tomorrow
;; https://github.com/daut/miasma-theme.el


;; wait to build with cairo use the above temporarily with the text caveat
;; (defun kb/toggle-window-transparency ()
;;   "Toggle transparency."
;;   (interactive)
;;   (let ((alpha-transparency 75))
;;     (pcase (frame-parameter nil 'alpha-background)
;;       (alpha-transparency (set-frame-parameter nil 'alpha-background 100))
;;       (t (set-frame-parameter nil 'alpha-background alpha-transparency)))))

;; (set-frame-parameter nil 'alpha-background 100) ; For current frame
;; (add-to-list 'default-frame-alist '(alpha-background . 100)) ; For all new frames henceforth


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
      (dape-buffer-default))))

;; terminates buffer automatically after inactivity of 30mins
(use-package buffer-terminator
  :ensure (:host github :url "https://github.com/jamescherti/buffer-terminator.el")
  :custom
  (buffer-terminator-verbose nil)
  :config
  (buffer-terminator-mode 1))

(defun jm/choose-font-size ()
  "Choose between three different font sizes: 16, 18, and 20."
  (interactive)
  (set-face-attribute 'default nil :height
                      (* 10 (string-to-number
                             (completing-read "Choose font size: "
                                              (mapcar #'number-to-string '(16 18 20)))))))

;; use this for now, maybe pair with https://codeberg.org/martianh/sentex
(setq sentence-end-double-space nil)

(use-package stillness-mode
  :config
  (stillness-mode +1))

(use-package vundo
  :commands (vundo)
  :custom
  (vundo-roll-back-on-quit nil)
  (vundo--window-max-height 10)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols))

(use-package pinentry
  :config
  (pinentry-start))

;; requires installation of cmake
(use-package vterm)

(use-package diff-hl
  :after
  magit
  :config
  (add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  (global-diff-hl-mode))

;; note to self: emacs goes freaking crazy when opening and resizing with aerospace

;; https://gist.github.com/pesterhazy/e8e445e6715f5d8bae3c62bc9db32469
;; in a monorepo does not correctly identify project root
(require 'cl-extra)

(setq project-sentinels '("package.json" "Gemfile" "global.d.ts"))

(defun find-enclosing-project (dir)
  (locate-dominating-file
   dir
   (lambda (file)
     (and (file-directory-p file)
          (cl-some (lambda (sentinel)
                     (file-exists-p (expand-file-name sentinel file)))
                   project-sentinels)))))


(add-hook 'project-find-functions
          #'(lambda (d)
              (let ((dir (find-enclosing-project d)))
                (if dir (list 'vc 'Git  dir) nil))))

(provide 'init-reynard)
