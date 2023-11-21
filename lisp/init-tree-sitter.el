(use-package treesit
  :ensure nil
  :init
  (setq treesit-language-source-alist
        '((ruby "https://github.com/tree-sitter/tree-sitter-ruby")
          (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
          (heex "https://github.com/phoenixframework/tree-sitter-heex")))

  (setq major-mode-remap-alist
        '((ruby-mode . ruby-ts-mode)))

  :config
  (message "treesit is loaded")

  (defun yejun/treesit-install-language-grammars ()
    "Build and install all the tree-sitter language grammar libraries
defined in `treesit-language-source-alist'"
    (interactive)
    (mapc #'treesit-install-language-grammar
          (mapcar #'car treesit-language-source-alist))))

(provide 'init-tree-sitter)
