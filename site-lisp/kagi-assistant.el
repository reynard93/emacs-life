;;; kagi-assistant.el -- Code for Kagi assistant bangs -*- lexical-binding: t -*-

;;; Commentary:
;;
;; https://help.kagi.com/kagi/features/bangs.html#kagi-assistant-bangs

;;; Code:

(require 'kagi-search)

(defun kagi-search--research ()
  (interactive)
  (kagi-search "!expert"))

(defun kagi-assistant-code ()
  (interactive)
  (kagi-search "!code"))

(defun kagi-assistant-chat ()
  (interactive)
  (kagi-search "!chat"))

(defun kagi-assistant-custom (&optional instruction)
  (interactive)
  (let ((search-type (if instruction
                         (concat "!custom" instruction)
                       "!custom")))
    (kagi-search search-type)))

(defvar-keymap kagi-assistant-keymap
  "r" #'kagi-assistant-research
  "d" #'kagi-assistant-code
  "c" #'kagi-assistant-chat
  "a" #'kagi-assistant-custom)

(provide 'kagi-assistant)
