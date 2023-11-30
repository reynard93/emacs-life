(use-package treesit
  :ensure nil
  :init
  (setq treesit-language-source-alist
        '((ruby "https://github.com/tree-sitter/tree-sitter-ruby")
          (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
          (heex "https://github.com/phoenixframework/tree-sitter-heex")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")))

  (defun yejun/treesit-install-language-grammars ()
    "Build and install all tree-sitter language grammar libraries"
    (interactive)
    (mapc #'treesit-install-language-grammar
          (mapcar #'car treesit-language-source-alist)))

  :config
  (message "treesit is loaded")

  :custom
  (major-mode-remap-alist
   '((ruby-mode . ruby-ts-mode)
     (css-mode . css-ts-mode)
     (javascript-mode . js-ts-mode)
     (js-json-mode . json-ts-mode)
     (conf-toml-mode . toml-ts-mode))))

(provide 'init-tree-sitter)
