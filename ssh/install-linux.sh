#!/usr/bin/env bash
# SSH with FIDO2 security keys on a Linux desktop. Safe to rerun.
#
# - ~/.ssh/config and the askpass helper are linked from this directory.
# - OpenSSH's own agent runs as the systemd user unit ssh-agent.service
#   (Debian's openssh-client) on $XDG_RUNTIME_DIR/openssh_agent, with the
#   drop-in ssh-agent-oshirase.conf.
# - The agent notifies through oshirase when a key waits for a touch;
#   oshirase is installed on demand by ../install-oshirase.sh.
# - ~/.zshenv points SSH_AUTH_SOCK at that agent, for every zsh including
#   non-interactive ones. The desktop may start others (gcr-ssh-agent,
#   gpg-agent's ssh socket) and whichever starts last wins the session's
#   SSH_AUTH_SOCK; the pin keeps zsh on the one that notifies.
#   ../.zshrc loads the keys.
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
unit=ssh-agent.service
dropin_dir="$HOME/.config/systemd/user/$unit.d"
dropin="$dropin_dir/oshirase.conf"

say() { printf '==> %s\n' "$*" >&2; }

if [[ "$(uname -s)" != Linux ]]; then
  say "This sets up a systemd user unit; run it on Linux."
  exit 1
fi
if ! systemctl --user show-environment >/dev/null 2>&1; then
  say "No systemd user manager; run this in the desktop session."
  exit 1
fi
if ! systemctl --user cat ssh-agent.socket >/dev/null 2>&1; then
  say "ssh-agent.socket is missing: it comes with openssh-client (Debian 12+)"
  exit 1
fi

"$here/../install-oshirase.sh" ||
  say "oshirase is not installed; the agent will wait for touches silently"

mkdir -p "$HOME/.ssh" "$HOME/.local/bin" "$dropin_dir"
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

systemctl --user enable --now ssh-agent.socket >/dev/null 2>&1

# Restart the agent only when the drop-in changed: a restart empties it, and
# the keys come back with the next new shell (.zshrc). A copy, not a link,
# so the comparison says whether systemd has seen this version.
if ! cmp -s "$here/ssh-agent-oshirase.conf" "$dropin"; then
  say "Installing $dropin"
  install -m 0644 "$here/ssh-agent-oshirase.conf" "$dropin"
  systemctl --user daemon-reload
  systemctl --user try-restart "$unit"
fi

if ! grep -qs 'SSH_AUTH_SOCK=.*openssh_agent' "$HOME/.zshenv"; then
  say "Pointing SSH_AUTH_SOCK at the agent in ~/.zshenv"
  cat >>"$HOME/.zshenv" <<'EOF'

# OpenSSH's agent (systemd: ssh-agent.socket), which notifies through oshirase
# when a security key waits for a touch; not gcr's or gpg-agent's.
export SSH_AUTH_SOCK="${XDG_RUNTIME_DIR:-/run/user/$UID}/openssh_agent"
EOF
fi

socket="${XDG_RUNTIME_DIR:-/run/user/$(id -u)}/openssh_agent"
if SSH_AUTH_SOCK="$socket" ssh-add -l >/dev/null 2>&1 ||
  [[ $? -eq 1 ]]; then # 1: running, no keys yet
  say "Agent running on $socket"
else
  say "The agent did not answer; see journalctl --user -u $unit"
  exit 1
fi
say "Open a new terminal to load the keys."
