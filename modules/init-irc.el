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

  (defun +irc/pull-chat-history ()
    (interactive)
    (circe-command-QUOTE
     (format "CHATHISTORY LATEST %s * 50" circe-chat-target)))

  (defun +irc/jump-to-channel ()
    (interactive)
    (let* ((channel-buffers (delq nil
                                  (mapcar (lambda (buf)
                                            (with-current-buffer buf
                                              (when (eq major-mode 'circe-channel-mode)
                                                (buffer-name buf))))
                                          (buffer-list))))
           (target (completing-read "Jump to channel: " channel-buffers nil t)))
      (when target
        (switch-to-buffer target))))

  :bind (("s-k" . +irc/jump-to-channel)
         :map circe-channel-mode-map
         ("C-c C-p" . +irc/pull-chat-history)))

(provide 'init-irc)
