;;; -*- lexical-binding: t -*-

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
  ;; (kill-emacs . dape-breakpoint-save)
  ;; Load breakpoints on startup
  ;; (after-init . dape-breakpoint-load)
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

;; need brew packages pinentry and pass, allows enter pw-store in minibuffer
;; add these two lines to your ~/.gnupg/gpg-agent.conf
;; allow-emacs-pinentry
;; allow-loopback-pinentry
;; then run `gpgcong --reload gpg-agent'
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
  :defer
  :hook((org-mode . org-download-enable)))

(use-package kubed
  :defer
  :bind-keymap ("C-c k" . kubed-prefix-map))

;; i love transparency but i cannot get alpha-background to work on the emacs i use
(use-package emacs
  :ensure nil
  :config
  (set-frame-parameter nil 'alpha '(92 . 85))
  (add-to-list 'default-frame-alist '(alpha . (92 . 85))))

;; TODO: was experiencing some issues creating .env when .env.example exists, keeps completing to .env.

(use-package tramp
  :ensure nil
  :custom
  (tramp-allow-unsafe-temporary-files t)
  :config
  ;; Make Tramp respect the remote server's PATH setting, so we can pick up
  ;; things like rbenv shims for remote Ruby execution via bundler
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; Consider https://github.com/copilot-emacs/copilot.el

(use-package mermaid-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.mermaid\\'" . mermaid-mode)))

(use-package prodigy
  :ensure t
  :bind
  ("C-c o o" . prodigy)
  :config
  (defun prodigy-start-if-not-running (service-name)
    "Start a service if it's not already running."
    (let ((service (prodigy-find-service service-name)))
      (unless (and service (prodigy-service-started-p service))
        (prodigy-start-service service))))

(prodigy-define-tag
  :name 'rails
  :on-output (lambda (&rest args)
               (let ((output (plist-get args :output))
                     (service (plist-get args :service)))
                 (when (or (s-matches? "Listening on 0\.0\.0\.0:[0-9]+, CTRL\\+C to stop" output)
                           (s-matches? "Ctrl-C to shutdown server" output))
                   (prodigy-set-status service 'ready)))))

(prodigy-define-tag
  :name 'foreman
  :ready-message "worker\\.[0-9]+\\s+\\|\\s+\\[Worker"
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "Spine Docker"
    :command "ssh"
    :args '("grain-spine@orb" "cd spine && docker-compose up")
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t
    :tags '(docker spine))

(prodigy-define-service
  :name "Spine Rails Server"
  :command "ssh"
  :args '("-t" "grain-spine@orb" "./start_spine_rails.sh")
  :stop-signal 'sigint
  :tags '(rails spine))

(prodigy-define-service
  :name "Papercut Foreman"
  :command "ssh"
  :args '("-t" "grain-spine@orb" "./papercut_foreman.sh")
  :tags '(foreman papercut))
  
  (global-set-key (kbd "C-c o s") (defun my/start-spine-docker () (interactive) (prodigy-start-if-not-running "Spine Docker")))
  (global-set-key (kbd "C-c o r") (defun my/start-spine-rails () (interactive) (prodigy-start-if-not-running "Spine Rails Server")))
  (global-set-key (kbd "C-c o p") (defun my/start-papercut () (interactive) (prodigy-start-if-not-running "Papercut Foreman")))
)

(use-package ox-gist)

; required for the experimental emacs build
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier 'meta)

;; requires pandoc installation: brew install pandoc
(defun markdown-to-org ()
  "Convert the current markdown buffer to org format using pandoc."
  (interactive)
  (let* ((md-file (buffer-file-name))
         (org-file (concat (file-name-sans-extension md-file) ".org")))
    (shell-command
     (format "pandoc -f markdown -t org \"%s\" -o \"%s\"" md-file org-file))
    (find-file org-file)))

;; Bind it to a key in markdown-mode
(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-map (kbd "C-c C-e o") 'markdown-to-org))


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


;; TODO: checkout polymode?

(provide 'init-reynard)
