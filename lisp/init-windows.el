(use-package winner
  :ensure nil
  :config
  (message "winner is loaded")
  (winner-mode 1))

(use-package ace-window
  :pin melpa
  :config
  (message "ace-window is loaded")

  ;; Open any buffer by splitting any window
  ;; https://karthinks.com/software/fifteen-ways-to-use-embark/#open-any-buffer-by-splitting-any-window
  (eval-when-compile
    (defmacro +embark--aw-action (fn)
      `(defun ,(intern (concat "+embark/aw-" (symbol-name fn))) ()
         ,(format "Open %s buffer selected with ace-window." (symbol-name fn))
         (interactive)
         (with-demoted-errors "%s"
           (require 'ace-window)
           (let ((aw-dispatch-always t))
             (aw-switch-to-window (aw-select nil))
             (call-interactively (symbol-function ',fn)))))))

  (with-eval-after-load 'embark
    (define-key embark-file-map     (kbd "o") (+embark--aw-action find-file))
    (define-key embark-buffer-map   (kbd "o") (+embark--aw-action switch-to-buffer))
    (define-key embark-bookmark-map (kbd "o") (+embark--aw-action bookmark-jump)))

  :custom
  (aw-scope 'frame)
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind ([remap other-window] . ace-window))

(use-package popper
  :config
  (message "popper is loaded")
  (popper-mode 1)
  (popper-echo-mode 1)
  :custom
  (popper-reference-buffers
   '(compilation-mode
     rspec-compilation-mode
     chatgpt-shell-mode
     osx-dictionary-mode
     "*Org Select*"
     "^CAPTURE-.*\\.org$")))

(provide 'init-windows)
