#!/bin/sh

cd "$(dirname "$0")/cinnamon" || exit 1

dconf load /org/cinnamon/desktop/keybindings/ < keybindings.conf
dconf load /org/gnome/libgnomekbd/ < libgnomekbd.conf
dconf load /org/cinnamon/desktop/peripherals/keyboard/ < keyboard.conf
