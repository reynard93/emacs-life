(use-package circe
  :pin melpa
  :init
  (setq circe-network-options
        '(("Libera Chat"
           :use-tls t
           :nick "goofansu"
           :realname "Yejun Su"
           :sasl-username "goofansu"
           :sasl-password (lambda (&rest _) (auth-source-pass-get 'secret "irc.libera.chat"))
           :channels ("#emacs" "#nixos" "#ruby" "#elixir"))))

  :bind ( :map goto-map
          ("K" . +circe/jump-to-channel))

  :config
  (circe-set-display-handler "353" 'circe-display-ignore)
  (circe-set-display-handler "366" 'circe-display-ignore)

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
