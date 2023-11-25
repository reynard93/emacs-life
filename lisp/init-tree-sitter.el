(use-package treesit
  :ensure nil
  :init
  (setq treesit-language-source-alist
        '((ruby "https://github.com/tree-sitter/tree-sitter-ruby")
          (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
          (heex "https://github.com/phoenixframework/tree-sitter-heex")))

  (defun yejun/treesit-install-language-grammars ()
    "Build and install all tree-sitter language grammar libraries"
    (interactive)
    (mapc #'treesit-install-language-grammar
          (mapcar #'car treesit-language-source-alist)))

  :config
  (message "treesit is loaded")

  :custom
  (major-mode-remap-alist '((ruby-mode . ruby-ts-mode))))

(provide 'init-tree-sitter)
