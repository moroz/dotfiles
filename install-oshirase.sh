#!/usr/bin/env bash
# Install oshirase (github.com/moroz/oshirase), the notification daemon that
# tells me when a security key is waiting for a touch or an agent needs me,
# if the pinned version is not already installed and running. Safe to rerun.
#
#   ./install-oshirase.sh           install $OSHIRASE_VERSION when needed
#   ./install-oshirase.sh --force   reinstall even when it is up to date
#
# The build itself is oshirase's own installer, with Go from its mise.toml:
# - macOS: install-macos.sh, Oshirase.app from Xcode and the launchd agent
#   dev.moroz.oshirase; logs in ~/Library/Logs/oshirase.log.
# - Linux: install-linux.sh, the systemd user service oshirase.service,
#   showing notifications through the desktop's freedesktop.org service;
#   settings in ~/.config/oshirase/env, logs in journalctl --user -u oshirase.
#   Skipped where there is no systemd user manager (a container, an SSH-only
#   box): the daemon belongs on the machine with the screen.
set -euo pipefail

# v0.3.0 is the first with Linux support; the macOS side is unchanged since
# v0.2.0.
version="${OSHIRASE_VERSION:-v0.5.0}"
binary="$HOME/.local/bin/oshirase"
src="${XDG_CACHE_HOME:-$HOME/.cache}/oshirase-src"
port=4649

say() { printf '==> %s\n' "$*" >&2; }

os="$(uname -s)"
case $os in
Darwin)
  installer=./install-macos.sh
  probe="127.0.0.1:$port"
  ;;
Linux)
  installer=./install-linux.sh
  if ! systemctl --user show-environment >/dev/null 2>&1; then
    say "No systemd user manager to run oshirase's daemon under; skipping"
    exit 0
  fi
  # The daemon listens where its env file says; the first address is
  # loopback unless it was set by hand. Same lookup as install-linux.sh.
  config_home="$(systemctl --user show-environment | sed -n 's/^XDG_CONFIG_HOME=//p')"
  env_file="${config_home:-$HOME/.config}/oshirase/env"
  # The file is not there before the first install.
  listen=""
  if [[ -f $env_file ]]; then
    listen="$(sed -n 's/^OSHIRASE_LISTEN=//p' "$env_file" | tail -1)"
  fi
  probe="${listen%%,*}"
  probe="${probe:-127.0.0.1:$port}"
  ;;
*)
  say "oshirase's daemon runs on macOS and Linux only; skipping"
  exit 0
  ;;
esac

if [[ "${1:-}" != --force && -x $binary ]] &&
  [[ "$("$binary" version 2>/dev/null)" == "$version" ]] &&
  curl -fsS "http://$probe/v1/health" >/dev/null 2>&1; then
  say "oshirase $version is installed and running"
  exit 0
fi

command -v mise >/dev/null || { say "mise is needed to build oshirase"; exit 1; }
command -v curl >/dev/null || { say "curl is needed for the health check"; exit 1; }
case $os in
Darwin)
  xcode-select -p >/dev/null 2>&1 || {
    say "Xcode command-line tools are needed: xcode-select --install"
    exit 1
  }
  ;;
Linux)
  # busctl comes with systemd; without it every notification is refused.
  command -v busctl >/dev/null || { say "busctl (systemd) is needed"; exit 1; }
  ;;
esac

say "Fetching oshirase $version"
rm -rf "$src"
mkdir -p "$(dirname "$src")"
# A shallow clone at the tag is enough: Go stamps the version from it.
git -c advice.detachedHead=false clone --quiet --depth 1 --branch "$version" \
  https://github.com/moroz/oshirase.git "$src"

cd "$src"
[[ -x $installer ]] || { say "oshirase $version has no $installer"; exit 1; }
mise trust mise.toml >/dev/null
mise install
mise exec -- "$installer"
