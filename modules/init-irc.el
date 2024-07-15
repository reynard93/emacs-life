(use-package circe
  :pin melpa
  :defer t
  :init
  (setq circe-network-defaults
        '(("chat.sr.ht/Libera.Chat"
           :host "chat.sr.ht"
           :port 6697
           :use-tls t
           :nick "goofansu"
           :realname "Yejun Su"
           :sasl-username "goofansu/irc.libera.chat"
           :sasl-password (lambda (&rest _) (auth-source-pass-get 'secret "chat.sr.ht"))
           :nickserv-password (lambda (&rest _) (auth-source-pass-get 'secret "irc.libera.chat")))))

  :config
  (message "circe is loaded")
  (circe-set-display-handler "353" 'circe-display-ignore)
  (circe-set-display-handler "366" 'circe-display-ignore)

  (defun +irc--channels ()
    (delq nil
          (mapcar (lambda (buf)
                    (with-current-buffer buf
                      (when (eq major-mode 'circe-channel-mode)
                        (buffer-name buf))))
                  (buffer-list))))

  (defun +irc/jump-to-channel ()
    (interactive)
    (let* ((channel-buffers (+irc--channels))
           (target (completing-read "Jump to channel: " channel-buffers nil t)))
      (when target
        (switch-to-buffer target))))

  (defun +irc/kill-all-channels ()
    (interactive)
    (mapc 'kill-buffer (+irc--channels)))

  :bind (("C-c I" . +irc/jump-to-channel)))

(provide 'init-irc)
