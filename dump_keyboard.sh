#!/bin/sh

# Dump Cinnamon keyboard settings (shortcuts, layouts, xkb options, repeat rate)
cd "$(dirname "$0")/cinnamon" || exit 1

dump() {
  echo "# load with dconf load $1 < $2" > "$2"
  echo >> "$2"
  dconf dump "$1" >> "$2"
}

dump /org/cinnamon/desktop/keybindings/ keybindings.conf
dump /org/gnome/libgnomekbd/ libgnomekbd.conf
dump /org/cinnamon/desktop/peripherals/keyboard/ keyboard.conf
