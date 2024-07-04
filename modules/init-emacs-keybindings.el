(bind-key "C-c t f"  #'flymake-mode)
(bind-key "C-c t s"  #'flyspell-mode)
(bind-key "C-c t t"  #'+theme-buffet/toggle)
(bind-key "C-c t v"  #'visible-mode)
(bind-key "C-c t w"  #'visual-line-mode)
(bind-key "C-c t z"  #'logos-focus-mode)

(bind-key "s" #'+project/search 'search-map)
(bind-key "S" #'+project/search-for-symbol-at-point 'search-map)

(bind-key "C-c y f"  #'+buffer/yank-path)
(bind-key "C-c y F"  #'+buffer/yank-path-relative-to-project)

(bind-key "C-c o p" #'+gh/pr-browse-at-remote)

(unbind-key "C-x f")
(bind-key "C-x f b"  #'yejun/browse-blog)
(bind-key "C-x f e"  #'yejun/browse-emacs-config)
(bind-key "C-x f n"  #'yejun/browse-nix-config)
(bind-key "C-x f p"  #'+tempel/find-private-template)
(bind-key "C-x f D"  #'+file/delete-this-file)
(bind-key "C-x f R"  #'+file/move-this-file)
(bind-key "C-x f f"  #'apheleia-format-buffer)
(bind-key "C-x f w"  #'delete-trailing-whitespace)

(with-eval-after-load 'rspec-mode-map
  (bind-key "C-c C-t a" #'rspec-verify-all 'rspec-mode-map)
  (bind-key "C-c C-t s" #'rspec-verify-single 'rspec-mode-map)
  (bind-key "C-c C-t v" #'rspec-verify 'rspec-mode-map)
  (bind-key "C-c C-t r" #'rspec-rerun 'rspec-mode-map)
  (bind-key "C-c C-t l" #'rspec-run-last-failed 'rspec-mode-map)
  (bind-key "C-c C-t e" #'rspec-toggle-example-pendingness 'rspec-mode-map)
  (bind-key "C-c C-t t" #'rspec-toggle-spec-and-target 'rspec-mode-map))

(provide 'init-emacs-keybindings)
