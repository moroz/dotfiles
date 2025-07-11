#!/bin/sh

light_theme="Tempus Fugit"
dark_theme="Tempus Winter"

theme="$(dconf read /org/x/apps/portal/color-scheme)"

if [ "$theme" = "'prefer-light'" ]; then
  gnome-terminal --profile="$light_theme" $@
else
  gnome-terminal --profile="$dark_theme" $@
fi
