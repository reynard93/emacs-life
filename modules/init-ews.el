;; copied over on 17 mar 2025 with commit hash

;; (ews-missing-executables
;;  '(("gs" "mutool")
;;    "pdftotext"
;;    "soffice"
;;    "zip"
;;    "ddjvu"
;;    "curl"
;;    ("mpg321" "ogg123" "mplayer" "mpv" "vlc")
;;    ("grep" "ripgrep")
;;    ("convert" "gm")
;;    "dvipng"
;;    "latex"
;;    "hunspell"
;;    "git"))

;;;###autoload
(defun ews-bibtex-register ()
  "Register the contents of the `ews-bibtex-directory' with `ews-bibtex-files`.
Use when adding or removing a BibTeX file from or to `ews-bibtex-directory'."
  (interactive)
  (when (file-exists-p ews-bibtex-directory)
    (let ((bib-files (directory-files ews-bibtex-directory t
				      "^[A-Z|a-z|0-9].+.bib$")))
      (setq ews-bibtex-files bib-files
  	    org-cite-global-bibliography bib-files
	    citar-bibliography bib-files)))
  (message "Registered:\n%s" (mapconcat #'identity ews-bibtex-files "\n")))

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
  :ensure nil
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

;; Export citations with Org Mode

(require 'oc-natbib)
(require 'oc-csl)

(setq org-cite-global-bibliography ews-bibtex-files
      org-cite-insert-processor 'citar
      org-cite-follow-processor 'citar
      org-cite-activate-processor 'citar)


;; Writegood-Mode for weasel words, passive writing and repeated word detection

(use-package writegood-mode
  :bind ; c for check?
  (("C-c w c r" . writegood-reading-ease)
   ("C-c w c l" . writegood-grade-level))
  :hook
  (text-mode . writegood-mode))

;; Titlecasing
;;;###autoload
(defun ews-org-headings-titlecase (&optional arg)
  "Cycle through all headings in an Org buffer and convert them to title case.
When used with universal argument (ARG) converts to sentence case.
Customise `titlecase-style' for styling."
  (interactive "P")
  (require 'titlecase)
  (let ((style (if arg 'sentence titlecase-style)))
    (message "Converting headings to '%s' style" style)
    (org-map-entries
     (lambda ()
       (let* ((heading (substring-no-properties (org-get-heading t t t t)))
	      (level (org-current-level))
	      (heading-lower (downcase heading))
              (new-heading (titlecase--string heading-lower style)))
	 (when (<= level (or ews-org-heading-level-capitalise 999))
	   (org-edit-headline new-heading)))))))

(use-package titlecase
  :bind
  (("C-c w t t" . titlecase-dwim)
   ("C-c w t c" . ews-org-headings-titlecase)))

;; epub export

(use-package ox-epub
  :demand t
  :init
  (require 'ox-org))

;; LaTeX PDF Export settings

(use-package ox-latex
  :ensure nil
  :demand t
  :custom
  ;; Multiple LaTeX passes for bibliographies
  (org-latex-pdf-process
   '("pdflatex -interaction nonstopmode -output-directory %o %f"
     "bibtex %b"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
  ;; Clean temporary files after export
  (org-latex-logfiles-extensions
   (quote ("lof" "lot" "tex~" "aux" "idx" "log" "out"
           "toc" "nav" "snm" "vrb" "dvi" "fdb_latexmk"
           "blg" "brf" "fls" "entoc" "ps" "spl" "bbl"
           "tex" "bcf"))))

;; EWS paperback configuration
(with-eval-after-load 'ox-latex
  (add-to-list
   'org-latex-classes
   '("ews"
     "\\documentclass[11pt, twoside, hidelinks]{memoir}
      \\setstocksize{9.25in}{7.5in}
      \\settrimmedsize{\\stockheight}{\\stockwidth}{*}
      \\setlrmarginsandblock{1.5in}{1in}{*}
      \\setulmarginsandblock{1in}{1.5in}{*}
      \\checkandfixthelayout
      \\layout
      \\setcounter{tocdepth}{0}
      \\setsecnumdepth{subsection}
      \\renewcommand{\\baselinestretch}{1.2}
      \\setheadfoot{0.5in}{0.75in}
      \\setlength{\\footskip}{0.8in}
      \\chapterstyle{bianchi}
      \\renewcommand{\\beforechapskip}{-30pt}
      \\setsecheadstyle{\\normalfont \\raggedright \\textbf}
      \\setsubsecheadstyle{\\normalfont \\raggedright \\emph}
      \\setsubsubsecheadstyle{\\normalfont\\centering}
      \\pagestyle{myheadings}
      \\usepackage[font={small, it}]{caption}
      \\usepackage{ccicons}
      \\usepackage{ebgaramond}
      \\usepackage[authoryear]{natbib}
      \\bibliographystyle{apalike}
      \\usepackage{svg}
      \\hyphenation{mini-buffer}
      \\renewcommand{\\LaTeX}{LaTeX}
      \\renewcommand{\\TeX}{TeX}"
     ("\\chapter{%s}" . "\\chapter*{%s}")
     ("\\section{%s}" . "\\section*{%s}")
     ("\\subsection{%s}" . "\\subsection*{%s}")
     ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

;; requires you to install gimp
;; Image viewer

(use-package emacs
  :ensure nil
  :custom
  (image-dired-external-viewer "gimp")
  :bind
  ((:map image-mode-map
         ("k" . image-kill-buffer)
         ("<right>" . image-next-file)
         ("<left>"  . image-previous-file))
   (:map dired-mode-map
         ("C-<return>" . image-dired-dired-display-external))))

(use-package image-dired
  :bind
  (("C-c w I" . image-dired))
  (:map image-dired-thumbnail-mode-map
        ("C-<right>" . image-dired-display-next)
        ("C-<left>"  . image-dired-display-previous)))



(provide 'init-ews)
