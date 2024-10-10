(use-package circe
  :pin melpa
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

  :bind ( :map goto-map
          ("K" . +circe/jump-to-channel)
          :map circe-channel-mode-map
          ("C-c C-p" . +circe/pull-recent-messages))

  :config
  (circe-set-display-handler "353" 'circe-display-ignore)
  (circe-set-display-handler "366" 'circe-display-ignore)

  (defun +circe/pull-recent-messages (limit)
    "Request up to LIMIT number of the most recent messages that have been sent."
    (interactive "sEnter the number of the most recent messages to request: ")
    (circe-command-QUOTE
     (format "CHATHISTORY LATEST %s * %s" circe-chat-target limit)))

  (defun +circe/jump-to-channel ()
    (interactive)
    (let* ((channel-buffers (delq nil
                                  (mapcar (lambda (buf)
                                            (with-current-buffer buf
                                              (when (eq major-mode 'circe-channel-mode)
                                                (buffer-name buf))))
                                          (buffer-list))))
           (target (completing-read "Select channel: " channel-buffers nil t)))
      (when target
        (switch-to-buffer target)))))

(provide 'init-irc)
