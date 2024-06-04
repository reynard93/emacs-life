(defvar +eww-package-endpoints
  '((elixir . "https://hex.pm/packages/")
    (node . "https://www.npmjs.com/package/")
    (ruby . "https://rubygems.org/gems/"))
  "A mapping of languages to their package endpoints.")

(defun +eww/browse-package (language package)
  "Browse PACKAGE in its LANGUAGE's websites using EWW."
  (interactive (list (intern (completing-read "Language: " (mapcar 'car +eww-package-endpoints) nil t))
                     (read-string "Package: ")))
  (let ((endpoint (cdr (assoc language +eww-package-endpoints)))
        (name (replace-regexp-in-string "[\"']" "" package)))
    (eww (concat endpoint name))))

(defun +eww/browse-package-ruby (target)
  (+eww/browse-package 'ruby target))

(defun +eww/browse-package-node (target)
  (+eww/browse-package 'node target))

(defun +eww/browse-package-elixir (target)
  (+eww/browse-package 'elixir target))

(with-eval-after-load 'embark
  (keymap-set embark-identifier-map "b e" #'+eww/browse-package-elixir)
  (keymap-set embark-identifier-map "b n" #'+eww/browse-package-node)
  (keymap-set embark-identifier-map "b r" #'+eww/browse-package-ruby)
  (keymap-set embark-expression-map "b e" #'+eww/browse-package-elixir)
  (keymap-set embark-expression-map "b n" #'+eww/browse-package-node)
  (keymap-set embark-expression-map "b r" #'+eww/browse-package-ruby))

(provide 'lisp-eww)
