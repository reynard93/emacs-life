# macOS Emacs client launcher (this folder)

This folder contains a small macOS-friendly wrapper for `emacsclient`.
It assumes your Emacs config starts the server (i.e. `(server-start)` happens during init), and it will try to start Emacs automatically when the server is not reachable.

## Files

- `emacsclient-macos`
  - Command-line launcher.
  - Opens a *new GUI frame* using: `emacsclient -n -c`.
  - If the Emacs server isn’t reachable, it will try to start Emacs via `open` and then retry.

- `Emacs Client.app/`
  - Double-clickable app bundle so you can launch a new client frame from Finder / Spotlight / Dock.
  - `Contents/MacOS/EmacsClient` is the actual shell launcher.
  - `Contents/Info.plist` is minimal bundle metadata.

- `setup-emacs-client-macos.sh`
  - One-shot setup script for a new Mac.
  - Installs/copies `Emacs Client.app` into `/Applications` (or a custom directory).
  - Creates a symlink to `emacsclient-macos` in `~/.local/bin` (or a custom directory) so it’s easy to run from a terminal.
  - Optionally installs Emacs via Homebrew if `emacsclient` is missing.

## Install / Setup on a new Mac

From this repo:

```bash
cd ~/.config/emacs
bash bin/setup-emacs-client-macos.sh --force
```

Optional: also install Emacs via Homebrew if needed:

```bash
bash bin/setup-emacs-client-macos.sh --force --install-emacs
```

After that:
- Finder: open **/Applications/Emacs Client.app**
- Terminal: run `emacsclient-macos`

If `~/.local/bin` isn’t on your `PATH`, add this to your shell rc file:

```bash
export PATH="$HOME/.local/bin:$PATH"
```

## Notes / portability

- The launchers try these methods to start Emacs (in order):
  1. `open -b org.gnu.Emacs`
  2. `open -a "Emacs"`
  3. `open -a "Emacs Mac Port"`

If your Emacs app uses a different bundle-id/name, adjust the `open ...` line in:
- `emacsclient-macos`
- `Emacs Client.app/Contents/MacOS/EmacsClient`
