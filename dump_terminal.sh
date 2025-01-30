#!/bin/sh

echo '# load with dconf load /org/gnome/terminal/legacy/profiles:/ < gnome-terminal.conf' > gnome-terminal.conf
echo >> gnome-terminal.conf
dconf dump /org/gnome/terminal/legacy/profiles:/ >> gnome-terminal.conf
