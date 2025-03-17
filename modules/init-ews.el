;; copied over on 17 mar 2025 with commit hash

(ews-missing-executables
 '(("gs" "mutool")
   "pdftotext"
   "soffice"
   "zip"
   "ddjvu"
   "curl"
   ("mpg321" "ogg123" "mplayer" "mpv" "vlc")
   ("grep" "ripgrep")
   ("convert" "gm")
   "dvipng"
   "latex"
   "hunspell"
   "git"))

(use-package mixed-pitch
  :hook
  (org-mode . mixed-pitch-mode))

;; Window management
;; Split windows sensibly
(setq split-width-threshold 120
      split-height-threshold nil)
;; 1. `split-width-threshold` to 120, meaning Emacs will prefer vertical splitting (side-by-side windows) when the frame is at least 120 columns wide
;; 2. `split-height-threshold` to nil, disabling automatic horizontal splitting (windows stacked on top of each other)
;; This encourages Emacs to create side-by-side windows rather than stacking them vertically, as long as there's enough horizontal space.

;; Read ePub files

(use-package nov
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

;; Managing Bibliographies

(use-package bibtex
  :custom
  (bibtex-user-optional-fields
   '(("keywords" "Keywords to describe the entry" "")
     ("file"     "Relative or absolute path to attachments" "" )))
  (bibtex-align-at-equal-sign t)
  :config
  (ews-bibtex-register)
  :bind
  (("C-c w b r" . ews-bibtex-register)))

;; Biblio package for adding BibTeX records

(use-package biblio
  :bind
  (("C-c w b b" . ews-bibtex-biblio-lookup)))

;; Don't need emms
;; Easy insertion of weblinks

(use-package org-web-tools
  :bind
  (("C-c w w" . org-web-tools-insert-link-for-url)))

;; Emacs Multimedia System

;; (use-package emms
;;   :config
;;   (require 'emms-setup)
;;   (require 'emms-mpris)
;;   (emms-all)
;;   (emms-default-players)
;;   (emms-mpris-enable)
;;   :custom
;;   (emms-browser-covers #'emms-browser-cache-thumbnail-async)
;;   :bind
;;   (("C-c w m b" . emms-browser)
;;    ("C-c w m e" . emms)
;;    ("C-c w m p" . emms-play-playlist )
;;    ("<XF86AudioPrev>" . emms-previous)
;;    ("<XF86AudioNext>" . emms-next)
;;    ("<XF86AudioPlay>" . emms-pause)))

(use-package openwith
  :config
  (openwith-mode t)
  :custom
  (openwith-associations nil))

(provide 'init-ews)
