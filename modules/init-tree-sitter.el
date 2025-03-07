;; ref here if need update https://evan.carlin.com/typescript-tsx-mode-for-emacs
(use-package treesit
  :ensure nil
  :init
  (setq treesit-language-source-alist
        '((c "https://github.com/tree-sitter/tree-sitter-c")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elixir "https://github.com/elixir-lang/tree-sitter-elixir")
          (heex "https://github.com/phoenixframework/tree-sitter-heex")
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          )

        )

  :custom
  (major-mode-remap-alist
   '((ruby-mode . ruby-ts-mode)
     (css-mode . css-ts-mode)
     (javascript-mode . js-ts-mode)
     (json-mode . json-ts-mode)
     (conf-toml-mode . toml-ts-mode)
     (c-mode . c-ts-mode)
     ("\\.[jt]s[x]?\\'" . tsx-mode)))

  :config
  (defun my/treesit-install-language-grammars ()
    "Build and install all the tree-sitter language grammar
 libraries defined in `treesit-language-source-alist'."
    (interactive)
    (mapc #'treesit-install-language-grammar
          (mapcar #'car treesit-language-source-alist))))

(provide 'init-tree-sitter)
