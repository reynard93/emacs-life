(use-package denote
  :defer t
  :init
  (setq denote-directory "~/src/notes")
  :config
  (message "denote is loaded")
  (require 'denote-org-extras)
  :custom
  (denote-history-completion-in-prompts nil)
  (denote-date-prompt-use-org-read-date t)
  (denote-known-keywords '("emacs" "nix" "ruby" "elixir" "webdev"))
  (denote-templates
   `((jira . ,(concat "Jira: "
                      "\n\n"
                      "* Problem"
                      "\n\n"
                      "* Investigation"
                      "\n\n"
                      "* Solution"
                      "\n\n"))
     )))

(use-package consult-denote
  :vc (consult-denote :url "https://github.com/protesilaos/consult-denote.git")
  :after consult
  :config
  (message "consult-denote is loaded")
  :custom
  (consult-denote-grep-command #'consult-ripgrep))

(provide 'init-denote)
