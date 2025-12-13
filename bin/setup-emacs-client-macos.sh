#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

APP_SRC="$SCRIPT_DIR/Emacs Client.app"
APP_DEST_DIR="/Applications"
APP_DEST="$APP_DEST_DIR/Emacs Client.app"

BIN_SRC="$SCRIPT_DIR/emacsclient-macos"
LINK_DIR_DEFAULT="$HOME/.local/bin"

usage() {
  cat <<'EOF'
Usage: setup-emacs-client-macos.sh [options]

Installs the Finder app wrapper and a PATH-friendly command for opening new Emacs client frames.

Options:
  --app-dir DIR        Destination Applications directory (default: /Applications)
  --link-dir DIR       Directory to place a symlink to emacsclient-macos (default: ~/.local/bin)
  --force              Overwrite existing app/symlink
  --install-emacs      If emacsclient is missing and Homebrew exists, install Emacs via brew
  -h, --help           Show this help
EOF
}

APP_DIR="$APP_DEST_DIR"
LINK_DIR="$LINK_DIR_DEFAULT"
FORCE=0
INSTALL_EMACS=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --app-dir) APP_DIR="$2"; shift 2 ;;
    --link-dir) LINK_DIR="$2"; shift 2 ;;
    --force) FORCE=1; shift ;;
    --install-emacs) INSTALL_EMACS=1; shift ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown arg: $1" >&2; usage; exit 2 ;;
  esac
done

APP_DEST_DIR="$APP_DIR"
APP_DEST="$APP_DEST_DIR/Emacs Client.app"

if [[ "$(uname -s)" != "Darwin" ]]; then
  echo "This setup script is for macOS (Darwin)." >&2
  exit 1
fi

if [[ ! -d "$APP_SRC" ]]; then
  echo "Missing app bundle: $APP_SRC" >&2
  exit 1
fi

if [[ ! -f "$BIN_SRC" ]]; then
  echo "Missing launcher script: $BIN_SRC" >&2
  exit 1
fi

chmod +x "$BIN_SRC" "$APP_SRC/Contents/MacOS/EmacsClient"

EMACSCLIENT="${EMACSCLIENT:-$(command -v emacsclient || true)}"
if [[ -z "${EMACSCLIENT}" ]]; then
  for p in /opt/homebrew/bin/emacsclient /usr/local/bin/emacsclient; do
    [[ -x "$p" ]] && EMACSCLIENT="$p" && break
  done
fi

if [[ -z "${EMACSCLIENT}" && $INSTALL_EMACS -eq 1 ]]; then
  if command -v brew >/dev/null 2>&1; then
    brew install --cask emacs
    EMACSCLIENT="$(command -v emacsclient || true)"
  else
    echo "Homebrew not found; install Emacs manually, then re-run." >&2
    exit 1
  fi
fi

if [[ -z "${EMACSCLIENT}" ]]; then
  echo "Warning: emacsclient not found yet. Install Emacs and ensure emacsclient is on PATH." >&2
fi

if [[ ! -d "$APP_DEST_DIR" ]]; then
  echo "App destination dir does not exist: $APP_DEST_DIR" >&2
  exit 1
fi

if [[ -e "$APP_DEST" && $FORCE -ne 1 ]]; then
  echo "App already exists: $APP_DEST (use --force to overwrite)" >&2
else
  [[ -e "$APP_DEST" && $FORCE -eq 1 ]] && rm -rf "$APP_DEST"
  ditto "$APP_SRC" "$APP_DEST"
  echo "Installed: $APP_DEST"
fi

mkdir -p "$LINK_DIR"
LINK_PATH="$LINK_DIR/emacsclient-macos"

if [[ -e "$LINK_PATH" && $FORCE -ne 1 ]]; then
  echo "Symlink already exists: $LINK_PATH (use --force to overwrite)" >&2
else
  ln -sf "$BIN_SRC" "$LINK_PATH"
  echo "Linked: $LINK_PATH -> $BIN_SRC"
fi

cat <<EOF

Done.
- Launch from Finder: $APP_DEST
- Launch from terminal: $LINK_PATH

If '$LINK_DIR' is not on your PATH, add this to your shell rc file:
  export PATH="$LINK_DIR:\$PATH"
EOF
