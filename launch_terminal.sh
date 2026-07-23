#!/bin/sh

light_theme="Solarized Light"
dark_theme="Zenburn"

theme="$(dconf read /org/x/apps/portal/color-scheme)"

if [ "$theme" = "'prefer-light'" ]; then
  gnome-terminal --profile="$light_theme" $@
else
  gnome-terminal --profile="$dark_theme" $@
fi
