(use-package circe
  :pin melpa
  :defer t
  :init
  (setq circe-network-options
        '(("sourcehut/liberachat"
           :host "chat.sr.ht"
           :port 6697
           :use-tls t
           :nick "goofansu"
           :realname "Yejun Su"
           :sasl-username "goofansu/irc.libera.chat"
           :sasl-password (lambda (&rest _) (auth-source-pick-first-password :host "chat.sr.ht"))
           :nickserv-password (lambda (&rest _) (auth-source-pick-first-password :host "irc.libera.chat")))))
  :config
  (message "circe is loaded")
  (circe-set-display-handler "353" 'circe-display-ignore)
  (circe-set-display-handler "366" 'circe-display-ignore)
  :bind (("s-k" . yejun/vertico-jump-to-channel)
         :map circe-channel-mode-map
         ("C-c C-p" . yejun/pull-chat-history)))

(defun yejun/pull-chat-history ()
  (interactive)
  (circe-command-QUOTE
   (format "CHATHISTORY LATEST %s * 50" circe-chat-target)))

;; https://github.com/doomemacs/doomemacs/blob/986398504d09e585c7d1a8d73a6394024fe6f164/modules/app/irc/autoload/vertico.el
(defun yejun/vertico-jump-to-channel ()
  "Jump to an open channel or server buffer with vertico."
  (interactive)
  (consult--multi (list (plist-put (copy-sequence yejun/irc--consult-circe-source)
                                   :hidden nil))
                  :narrow nil
                  :require-match t
                  :prompt "Jump to:"
                  :sort nil))

(defvar yejun/irc--consult-circe-source
        `(:name     "circe"
          :hidden   t
          :narrow   ?c
          :category buffer
          :state    ,#'consult--buffer-state
          :items    ,(lambda () (mapcar #'buffer-name (yejun/irc--circe-all-buffers)))))

;; https://github.com/doomemacs/doomemacs/blob/986398504d09e585c7d1a8d73a6394024fe6f164/modules/app/irc/autoload/irc.el#L76-L83
(defun yejun/irc--circe-all-buffers ()
  (cl-loop with servers = (circe-server-buffers)
           for server in servers
           collect server
           nconc
           (with-current-buffer server
             (cl-loop for buf in (circe-server-chat-buffers)
                      collect buf))))

(provide 'init-irc)
