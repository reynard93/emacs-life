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

(defgroup ews ()
  "Emacs Writing Studio."
  :group 'files
  :link '(url-link :tag "Homepage" "https://lucidmanager.org/tags/emacs/"))

(defcustom ews-hunspell-dictionaries "en_US"
  "Comma-separated list of Hunspell dictionaries."
  :group 'ews
  :type 'list)

(defcustom ews-org-heading-level-capitalise nil
  "Minimum level of Org headings to be capitalised
Nil implies all levels are capitalised."
  :group 'ews
  :type  '(choice (const :tag "All headings" nil)
		  (integer :tag "Highest level" 1)))

;; Check for missing external software
;;;###autoload
(defun ews-missing-executables (prog-list)
  "Identify missing executables in PROG-LIST.
Sublists indicate that one of the entries is required."
  (let ((missing '()))
    (dolist (exec prog-list)
      (if (listp exec)
          (unless (cl-some #'executable-find exec)
            (push (format "(%s)" (mapconcat 'identity exec " or ")) missing))
        (unless (executable-find exec)
          (push exec missing))))
    (if missing
        (message "Missing executable files(s): %s"
                 (mapconcat 'identity missing ", "))
      (message "No missing executable files."))))

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

;;;###autoload
(defun ews-bibtex-biblio-lookup ()
  "Insert Biblio search results into current buffer or select BibTeX file."
  (interactive)
  (if-let ((current-mode major-mode)
	   ews-bibtex-files
	   (bibfiles (length ews-bibtex-files))
	   (bibfile (cond ((eq bibfiles 1) (car ews-bibtex-files))
			  ((equal major-mode 'bibtex-mode)
			   (buffer-file-name))
			  (t (completing-read
			      "Select BibTeX file:" ews-bibtex-files)))))
      (progn (find-file bibfile)
	     (goto-char (point-max))
	     (ews--bibtex-combined-biblio-lookup)
	     (save-buffer))
    (message "No BibTeX file(s) defined.")))

;; Search for missing BibTeX attachments and filenames
(defun ews--bibtex-extract-attachments ()
  "Extract attachment file names from BibTeX files in `ews-bibtex-directory'."
  (ews-bibtex-register)
  (let ((attachments '()))
    (dolist (bibtex-file ews-bibtex-files)
      (with-temp-buffer
        (insert-file-contents bibtex-file)
        (goto-char (point-min))
        (while (re-search-forward "file.*=.*{\\([^}]+\\)}" nil t)
          (let ((file-paths (split-string (match-string 1)
                                          "[[:space:]]*;[[:space:]]*")))
            (dolist (file-path file-paths)
              (push (expand-file-name (string-trim file-path)
                                      ews-bibtex-directory)
                    attachments))))))
    attachments))

(defun ews--bibtex-extract-files ()
  "List files recursively in `ews-bibtex-directory', excluding `.bib' and `.csl'."
  (seq-remove (lambda (file)
                (or (string-suffix-p ".bib" file)
                    (string-suffix-p ".csl" file)))
              (mapcar 'expand-file-name
                      (directory-files-recursively ews-bibtex-directory ""))))

;;;###autoload
(defun ews-bibtex-missing-files ()
  "List BibTeX attachments not listed in a BibTeX file entry."
  (interactive)
  (let* ((files (ews--bibtex-extract-files))
         (attachments (ews--bibtex-extract-attachments))
         (missing (cl-remove-if
                   (lambda (f) (member f attachments)) files)))
    (message "%s files not registered in bibliography" (length missing))
    (dolist (file missing)
      (message file))))

;;;###autoload
(defun ews-bibtex-missing-attachments ()
  "List BibTeX file entries with missing attachment(s)."
  (interactive)
  (let* ((files (ews--bibtex-extract-files))
         (attachments (ews--bibtex-extract-attachments))
         (missing (cl-remove-if
                   (lambda (f) (member f files)) attachments)))
    (message "%s BibTeX files without matching attachment." (length missing))
    (dolist (file missing)
      (message file))))

;;;###autoload
(defun ews-org-insert-notes-drawer ()
  "Generate or open a NOTES drawer under the current heading.
If a drawer exists for this section, a new line is created at the end of the
current note."
  (interactive)
  (push-mark)
  (org-previous-visible-heading 1)
  (forward-line)
  (if (looking-at-p "^[ \t]*:NOTES:")
      (progn
        (org-fold-hide-drawer-toggle 'off)
        (re-search-forward "^[ \t]*:END:" nil t)
        (forward-line -1)
        (org-end-of-line)
        (org-return))
    (org-insert-drawer nil "NOTES"))
  (org-unlogged-message "Press <C-u C-SPACE> to return to the previous position."))

;;;###autoload
(defun ews-org-count-words ()
  "Add word count to each heading property drawer in an Org mode buffer."
  (interactive)
  (org-map-entries
   (lambda ()
     (let* ((start (point))
            (end (save-excursion (org-end-of-subtree)))
            (word-count (count-words start end)))
       (org-set-property "WORDCOUNT" (number-to-string word-count))))))


(defun ews-denote-link-description-title-case (file)
  "Return link description for FILE.

If the region is active, use it as the description.
The title is formatted with the `titlecase' package.

This function is useful as the value of `denote-link-description-function' to
generate links in titlecase for attachments."
  (require 'titlecase)
  (let* ((file-type (denote-filetype-heuristics file))
         (title (denote-retrieve-title-or-filename file file-type))
	 (clean-title (if (string-match-p " " title)
			  title
			(replace-regexp-in-string "\\([a-zA-Z0-9]\\)-\\([a-zA-Z0-9]\\)" "\\1 \\2" title)))
         (region-text (denote--get-active-region-content)))
    (cond
     (region-text region-text)
     (title (format "%s" (titlecase--string clean-title titlecase-style)))
     (t ""))))



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
  :ensure nil
  :bind
  (("C-c w I" . image-dired))
  (:map image-dired-thumbnail-mode-map
        ("C-<right>" . image-dired-display-next)
        ("C-<left>"  . image-dired-display-previous)))



(provide 'init-ews)
