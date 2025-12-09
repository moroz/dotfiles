#!/usr/bin/env -S bash -euxo pipefail

SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

pacman -Qqne > ${SCRIPT_DIR}/pkglist.txt
pacman -Qqem > ${SCRIPT_DIR}/aurlist.txt
