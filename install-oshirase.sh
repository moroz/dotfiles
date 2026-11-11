#!/usr/bin/env bash
# Install oshirase (github.com/moroz/oshirase), the notification daemon that
# tells me when a security key is waiting for a touch, if the pinned version
# is not already installed and running. Safe to rerun.
#
#   ./install-oshirase.sh           install $OSHIRASE_VERSION when needed
#   ./install-oshirase.sh --force   reinstall even when it is up to date
#
# The build itself is oshirase's own install-macos.sh: Go from its mise.toml,
# Oshirase.app from Xcode, the launchd agent dev.moroz.oshirase.
set -euo pipefail

version="${OSHIRASE_VERSION:-v0.2.0}"
binary="$HOME/.local/bin/oshirase"
src="${XDG_CACHE_HOME:-$HOME/.cache}/oshirase-src"

say() { printf '==> %s\n' "$*" >&2; }

if [[ "$(uname -s)" != Darwin ]]; then
  say "oshirase's daemon runs on macOS only; skipping"
  exit 0
fi

if [[ "${1:-}" != --force && -x $binary ]] &&
  [[ "$("$binary" version 2>/dev/null)" == "$version" ]] &&
  curl -fsS http://127.0.0.1:4649/v1/health >/dev/null 2>&1; then
  say "oshirase $version is installed and running"
  exit 0
fi

command -v mise >/dev/null || { say "mise is needed to build oshirase"; exit 1; }
xcode-select -p >/dev/null 2>&1 || {
  say "Xcode command-line tools are needed: xcode-select --install"
  exit 1
}

say "Fetching oshirase $version"
rm -rf "$src"
mkdir -p "$(dirname "$src")"
# A shallow clone at the tag is enough: Go stamps the version from it.
git -c advice.detachedHead=false clone --quiet --depth 1 --branch "$version" \
  https://github.com/moroz/oshirase.git "$src"

cd "$src"
mise trust mise.toml >/dev/null
mise install
mise exec -- ./install-macos.sh
