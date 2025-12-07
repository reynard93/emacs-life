;;; -*- lexical-binding: t -*-

(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

(defvar my-src-directory "~/src/"
  "Directory for source code repos.")

;; Performance optimizations should be loaded first
(require 'init-performance)

;; Bootstrap
(require 'init-bootstrap)
(require 'init-emacs)

;; Features
(require 'init-highlight)
(require 'init-git)
(require 'init-gpt)
(require 'init-pass)
(require 'init-search)
(require 'init-shell-command)
(require 'init-syntax-checker)

;; Contexts
(require 'init-programming)
(require 'init-howm)
(require 'init-writing)

;; Services
(require 'init-reynard)
