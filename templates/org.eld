org-mode

(begin & "#+begin_" (s name) n r n "#+end_" name)
(quote & "#+begin_quote" n r n "#+end_quote")
(src & "#+begin_src " p n r n "#+end_src" :post (org-edit-src-code))
(elisp & "#+begin_src emacs-lisp" n r n "#+end_src")
(nix & "#+begin_src nix" n r n "#+end_src")
(rest & "#+begin_src restclient" n r n "#+end_src")
(ruby & "#+begin_src ruby" n r n "#+end_src")
