#!/bin/bash
# Override the i915 GuC and DMC firmware with Debian 12's versions
# (firmware-misc-nonfree 20230210-5): GuC 70.5.1, DMC v2.18.
#
# The kernel looks in /lib/firmware/updates before /lib/firmware, so the
# Debian 13 firmware packages stay installed and untouched. Run as root.
#
# Undo: rm -r /lib/firmware/updates/i915 && update-initramfs -u -k all
set -euo pipefail

deb=firmware-misc-nonfree_20230210-5_all.deb
url=http://deb.debian.org/debian/pool/non-free-firmware/f/firmware-nonfree/$deb

declare -A sha256=(
	[adlp_guc_70.bin]=4aa024e6dab817d46fee4a267504cb06ef927fd88bacc2d4baf769d062dcd1d6
	[adlp_dmc.bin]=81946a83b1eb9e0d192646fffbd41361b6764a08fb4115f9488f38eb6a21d831
)

work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT

curl -fsSL -o "$work/$deb" "$url"
dpkg-deb -x "$work/$deb" "$work/x"

for f in "${!sha256[@]}"; do
	src=$work/x/lib/firmware/i915/$f
	echo "${sha256[$f]}  $src" | sha256sum -c --quiet
	install -D -m 644 "$src" "/lib/firmware/updates/i915/$f"
done

update-initramfs -u -k all
sha256sum /lib/firmware/updates/i915/*
