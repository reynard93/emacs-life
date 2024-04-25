(use-package denote
  :defer t
  :init
  (setq denote-directory "~/src/notes")
  :config
  (message "denote is loaded")
  (require 'denote-org-extras)

  (defun +denote/scratch ()
    (interactive)
    (let ((denote-prompts nil))
      (call-interactively #'denote)))

  (defun +denote/template-with-subdirectory ()
    (interactive)
    (let ((denote-prompts '(template subdirectory title keywords)))
      (call-interactively #'denote)))

  :custom
  (denote-history-completion-in-prompts nil)
  (denote-date-prompt-use-org-read-date t)
  (denote-templates
   `((jira-bug . ,(concat "Jira: "
                          "\n\n"
                          "* Problem"
                          "\n\n"
                          "* Investigation"
                          "\n\n"
                          "* Solution"
                          "\n\n"))
     (jira-story . ,(concat "Jira: "
                            "\n\n"
                            "* Requirements"
                            "\n\n"
                            "* Research"
                            "\n\n"
                            "* Solution"
                            "\n\n"))
     )))

(use-package consult-denote
  :vc (consult-denote :url "https://github.com/protesilaos/consult-denote.git")
  :after (consult denote)
  :config
  (message "consult-denote is loaded")
  :custom
  (consult-denote-grep-command #'consult-ripgrep))

(provide 'init-denote)
