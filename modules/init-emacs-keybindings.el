(bind-key "C-c t f"  #'flymake-mode)
(bind-key "C-c t s"  #'flyspell-mode)
(bind-key "C-c t t"  #'+theme-buffet/toggle)
(bind-key "C-c t v"  #'visible-mode)
(bind-key "C-c t w"  #'visual-line-mode)
(bind-key "C-c t z"  #'logos-focus-mode)

(bind-key "s" #'+project/search 'search-map)
(bind-key "S" #'+project/search-for-symbol-at-point 'search-map)

(bind-key "C-c g p"   #'+gh/pr-view)
(bind-key "C-c g o p" #'+gh/pr-browse-at-remote)
(bind-key "C-c g c b" #'+git/create-backup-commit)
(bind-key "C-c g c p" #'+gh/pr-create)

(bind-key "C-c y"  #'+buffer/yank-path)
(bind-key "C-c Y"  #'+buffer/yank-path-relative-to-project)

(with-eval-after-load 'rspec-mode-map
  (bind-key "C-c C-t a" #'rspec-verify-all 'rspec-mode-map)
  (bind-key "C-c C-t s" #'rspec-verify-single 'rspec-mode-map)
  (bind-key "C-c C-t v" #'rspec-verify 'rspec-mode-map)
  (bind-key "C-c C-t r" #'rspec-rerun 'rspec-mode-map)
  (bind-key "C-c C-t l" #'rspec-run-last-failed 'rspec-mode-map)
  (bind-key "C-c C-t e" #'rspec-toggle-example-pendingness 'rspec-mode-map)
  (bind-key "C-c C-t t" #'rspec-toggle-spec-and-target 'rspec-mode-map))

(provide 'init-emacs-keybindings)
