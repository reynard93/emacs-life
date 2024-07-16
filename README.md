# emacs-config

## Requirement
- Emacs 29+

## Installation

Clone the repo and start Emacs.

```shell
git clone git@git.sr.ht:~goofansu/emacs-config ~/.config/emacs
```

If you're using Nix on macOS, you may need to import `package-keyring.gpg` to install packages from [ELPA](https://elpa.gnu.org/):
```
mkdir -p ~/.config/emacs/elpa/gnupg
gpg --homedir ~/.config/emacs/elpa/gnupg --import /nix/store/15q4wzvn0ganspgpa2icvj59nr65jhx0-emacs-29.3/share/emacs/29.3/etc/package-keyring.gpg
```

## Packages

The configuration is based on these packages:

- [use-package](https://www.gnu.org/software/emacs/manual/html_mono/use-package.html)
- [project](https://www.gnu.org/software/emacs/manual/html_node/emacs/Projects.html)
- [treesit](https://www.gnu.org/software/emacs/manual/html_node/elisp/Parsing-Program-Source.html)
- [eglot](https://www.gnu.org/software/emacs/manual/html_mono/eglot.html)
- [modus-themes](https://github.com/protesilaos/modus-themes)
- [spacious-padding](https://github.com/protesilaos/spacious-padding)
- [fontaine](https://github.com/protesilaos/fontaine)
- [vertico](https://github.com/minad/vertico)
- [marginalia](https://github.com/minad/marginalia)
- [consult](https://github.com/minad/consult)
- [orderless](https://github.com/oantolin/orderless)
- [embark](https://github.com/oantolin/embark)
- [corfu](https://github.com/minad/corfu)
- [cape](https://github.com/minad/cape)
- [tempel](https://github.com/minad/tempel)
- [denote](https://github.com/protesilaos/denote)

## Thanks

I learned a lot from the Emacs community, especially thanks to:

- [Daniel Mendler](https://github.com/minad)
- [David Wilson](https://systemcrafters.net/)
- [Henrik Lissner](https://github.com/hlissner)
- [Jonas Bernoulli](https://github.com/tarsius)
- [Karthik Chikmagalur](https://github.com/karthink)
- [Mickey Petersen](https://www.masteringemacs.org/)
- [Omar Antolín Camarena](https://github.com/oantolin)
- [Protesilaos Stavrou](https://protesilaos.com/)
- [Sacha Chua](https://sachachua.com/)
- [Álvaro Ramírez](https://xenodium.com/)
