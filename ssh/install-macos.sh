#!/usr/bin/env bash
# SSH with FIDO2 security keys on macOS. Safe to rerun.
#
# - ~/.ssh/config and the askpass helper are linked from this directory.
# - Homebrew's ssh-agent runs under launchd (dev.moroz.ssh-agent) on
#   ~/.ssh/agent.sock: Apple's agent can list -sk keys but not sign with them.
# - The agent notifies through oshirase when a key waits for a touch;
#   oshirase is installed on demand by ../install-oshirase.sh.
# - ~/.zshenv points SSH_AUTH_SOCK at the agent, for every zsh including
#   non-interactive ones (limactl, scripts). ../.zshrc loads the keys.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
label="dev.moroz.ssh-agent"
plist="$HOME/Library/LaunchAgents/$label.plist"
domain="gui/$(id -u)"

say() { printf '==> %s\n' "$*" >&2; }

if [[ "$(uname -s)" != Darwin ]]; then
  say "This sets up a launchd agent; run it on macOS."
  exit 1
fi

if [[ ! -x /opt/homebrew/bin/ssh-agent ]]; then
  say "Homebrew OpenSSH is missing: brew install openssh (it is in the Brewfile)"
  exit 1
fi

"$here/../install-oshirase.sh" ||
  say "oshirase is not installed; the agent will wait for touches silently"

mkdir -p "$HOME/.ssh" "$HOME/.local/bin" "$(dirname "$plist")"
chmod 700 "$HOME/.ssh"

# A config that is not ours yet is kept beside the link, never overwritten.
config="$HOME/.ssh/config"
if [[ -e $config && ! -L $config ]]; then
  backup="$config.before-dotfiles.$(date +%Y%m%d%H%M%S)"
  say "Keeping the existing ~/.ssh/config as $backup"
  mv "$config" "$backup"
fi
ln -sfn "$here/config" "$config"
ln -sfn "$here/ssh-askpass-oshirase" "$HOME/.local/bin/ssh-askpass-oshirase"

# Reload the agent only when its definition changed: a restart empties it,
# and the keys come back with the next new shell (.zshrc).
rendered="$(sed -e "s|__HOME__|$HOME|g" "$here/$label.plist")"
if [[ ! -f $plist ]] || [[ "$(cat "$plist")" != "$rendered" ]]; then
  say "Installing $plist"
  printf '%s\n' "$rendered" >"$plist"
  plutil -lint -s "$plist"
  launchctl bootout "$domain/$label" 2>/dev/null || true
  launchctl bootstrap "$domain" "$plist"
elif ! launchctl print "$domain/$label" >/dev/null 2>&1; then
  launchctl bootstrap "$domain" "$plist"
fi

if ! grep -qs 'SSH_AUTH_SOCK=.*agent.sock' "$HOME/.zshenv"; then
  say "Pointing SSH_AUTH_SOCK at the agent in ~/.zshenv"
  cat >>"$HOME/.zshenv" <<'EOF'

# Homebrew ssh-agent (launchd: dev.moroz.ssh-agent); Apple's can't sign with -sk keys.
export SSH_AUTH_SOCK="$HOME/.ssh/agent.sock"
EOF
fi

for _ in $(seq 20); do
  [[ -S $HOME/.ssh/agent.sock ]] && break
  sleep 0.25
done
if [[ -S $HOME/.ssh/agent.sock ]]; then
  say "Agent running on ~/.ssh/agent.sock"
else
  say "The agent did not start; see /tmp/$label.log"
  exit 1
fi
say "Open a new terminal to load the keys; Lima reconnects pick up the agent."
