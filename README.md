# emacs-config

## Requirement
- Emacs 29+

## Features

### Built-in packages

Emacs 29 offers a number of built-in packages, and I try to use them
as much as possible before using community packages:

- [use-package](https://www.gnu.org/software/emacs/manual/html_mono/use-package.html)
- [project](https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html)
- [treesit](https://www.gnu.org/software/emacs/manual/html_node/elisp/Parsing-Program-Source.html)
- [eglot](https://www.gnu.org/software/emacs/manual/html_mono/eglot.html)

Details can be found in [What's New in Emacs 29.1](https://www.masteringemacs.org/article/whats-new-in-emacs-29-1)
authored by [Mickey Petersen](https://www.masteringemacs.org/).

### Beautiful UI

These packages make Emacs pleasing to my eye:

- [modus-themes](https://git.sr.ht/~protesilaos/modus-themes)
- [spacious-padding](https://git.sr.ht/~protesilaos/spacious-padding)
- [fontaine](https://git.sr.ht/~protesilaos/fontaine)
- [doom-modeline](https://github.com/seagle0128/doom-modeline)

Thanks to [Protesilaos Stavrou](https://protesilaos.com/) and [Vincent Zhang](https://github.com/seagle0128).

### Minibuffer completion

The minibuffer completion is powered by the combination:

- [vertico](https://github.com/minad/vertico)
- [marginalia](https://github.com/minad/marginalia)
- [consult](https://github.com/minad/consult)
- [orderless](https://github.com/oantolin/orderless)
- [embark](https://github.com/oantolin/embark)

Credits to [Daniel Mendler](https://github.com/minad) and [Omar Antolín Camarena](https://github.com/oantolin) for the great work.

### In-buffer completion

The in-buffer completion is also created by [Daniel Mendler](https://github.com/minad), which replace the ubiquitous company and yasnippet:

- [corfu](https://github.com/minad/corfu)
- [cape](https://github.com/minad/cape)
- [tempel](https://github.com/minad/tempel)

### Evil keybindings

I use [evil](https://github.com/emacs-evil/evil) everywhere, and defined a lot of keybindings with the help of [general](https://github.com/noctuid/general.el).
Don't worry about key bindings though, as [which-key](https://github.com/justbur/emacs-which-key) can help.

### Note-taking

[Denote](https://git.sr.ht/~protesilaos/denote) is my go-to tool for note-taking, its [FAQ](https://protesilaos.com/emacs/denote#h:da2944c6-cde6-4c65-8f2d-579305a159bb) 
explains the design philosophy and I'm totally agree with [Protesilaos Stavrou](https://protesilaos.com/).

## Thanks

I learned a lot from the Emacs community, especially thanks to:

- [Doom Emacs](https://github.com/doomemacs/doomemacs) created by [Henrik Lissner](https://github.com/hlissner)
- [Doom Emacs Configuration](https://tecosaur.github.io/emacs-config/config.html) authored by [tecosaur](https://github.com/tecosaur)
- [Emacs From Scratch](https://systemcrafters.net/emacs-from-scratch/) created by [David Wilson](https://systemcrafters.net/)
- [Fifteen Ways To Use Embark](https://karthinks.com/software/fifteen-ways-to-use-embark/) authored by [Karthik Chikmagalur](https://github.com/karthink)
