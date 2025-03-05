;; https://matklad.github.io/2024/10/14/missing-ide-feature.html
;; uses tree-sitter, supported langs: rust, c++, js, python
(use-package auto-hide
  :vc (auto-hide :url "https://github.com/ultronozm/auto-hide.el")
  :custom
  (auto-hide-language-config
   (append auto-hide-language-config
           '((ruby-ts-mode . ((function-node . "method")
                              (body-field . "body_statement"))))))
  :config
  (global-auto-hide-mode))

;; start Embark new splitting ways
(defun split-and-follow-horizontally ()
  "Split window horizontally and move to the new window."
  (interactive)
  (select-window (split-window-below)))

(defun split-and-follow-vertically ()
  "Split window vertically and move to the new window."
  (interactive)
  (select-window (split-window-right)))

(defun split-and-follow-horizontally-and-open (file)
  "Split window horizontally, move to the new window, and open FILE."
  (split-and-follow-horizontally)
  (find-file file))

(defun split-and-follow-vertically-and-open (file)
  "Split window vertically, move to the new window, and open FILE."
  (split-and-follow-vertically)
  (find-file file))

(with-eval-after-load 'embark
  (define-key embark-file-map (kbd "X") #'split-and-follow-horizontally-and-open)
  (define-key embark-file-map (kbd "V") #'split-and-follow-vertically-and-open))
;; end Embark new splitting ways

;; Add undo-tree

(provide 'init-reynard)
